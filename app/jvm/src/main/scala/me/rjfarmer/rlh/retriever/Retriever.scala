package me.rjfarmer.rlh.retriever

import akka.actor._
import akka.io.IO
import me.rjfarmer.rlh.api.WebserviceResult
import org.ehcache.Cache
import spray.can.Http
import spray.can.Http.{HostConnectorInfo, HostConnectorSetup}
import spray.http.{StatusCode, HttpResponse, HttpRequest, Uri}
import spray.http.HttpMethods.GET

import scala.concurrent.duration._
import scala.util.Try


/**
 * A retrievable item.
 *
 * It has a key and an uri by which it can be retrieved.
 *
 * @tparam K key type
 */
trait Retrievable[K] {

  def key: K

  def httpGetUri: Uri

  def replyTo: Option[ActorRef]

}

/**
 * A groupp of retrievables.
 *
 * @tparam K key of retrievables
 */
trait RetriGroup[K] {

  def items: Vector[(K, Uri)]

  def replyTo: ActorRef

  def retrievable(k: K, uri: Uri, replyTo: Option[ActorRef]): Retrievable[K]

}

/**
 * Scala wrapped API for EHC Cache
 *
 * Necessary because the key values for long need to be java.lang.Long
 * which is different from Long.
 *
 * Also, working with options is nicer
 *
 * @tparam K key type
 * @tparam V value type
 */
trait RetrieveCache[K,V <: WebserviceResult] {

  def get(k: K): Option[V]

  def put(k: K, v: V): Unit

}

/** ehc cache with long keys */
class EhcRetrieveLongCache[V <: WebserviceResult] (cache: Cache[java.lang.Long, V])extends RetrieveCache[Long, V] {

  def get(k: Long): Option[V] = Option(cache.get(k))

  def put(k: Long, v: V): Unit = cache.put(k, v)

}


trait BodyParser[V] {

  def parseBody(body: String): V

}


object Retriever {

  def props[K, V <: WebserviceResult](cache: RetrieveCache[K, V],
                                      queue: RetrieveQueue[K],
                                      parser: BodyParser[V],
                                      hostConnectorSetup: HostConnectorSetup): Props =
    Props(new Retriever[K, V](cache, queue, parser, hostConnectorSetup))

}

class Retriever[K,V <: WebserviceResult] (cache: RetrieveCache[K, V],
                      queue: RetrieveQueue[K],
                      parser: BodyParser[V],
                      hostConnectorSetup: HostConnectorSetup) extends Actor with ActorLogging {

  import me.rjfarmer.rlh.server.Boot.bootSystem

  private[this] var hostConnector: Option[ActorRef] = None
  private[this] var activeRequest: Option[Retrievable[K]] = None
  private[this] var activeRequestStarted: Long = 0L

  override def receive: Receive = {

    case group: RetriGroup[K] =>
      val (cached, uncached, stale) = cachedAndNeedToRefresh(group.items)
      val collector = context.actorOf(Collector.props(cache, cached, uncached.size, group.replyTo, 15.seconds))
      uncached.foreach { keyAndUri =>
        val reply = Some(collector)
        self ! group.retrievable(keyAndUri._1, keyAndUri._2, reply)
      }
      stale.foreach { keyAndUri =>
        self ! group.retrievable(keyAndUri._1, keyAndUri._2, None)
      }

    case item : Retrievable[K] =>
      answerCachedOrElse(item, queueRetrieve)

    case HostConnectorInfo(hc, _) =>
      log.debug("hostConnector {}", hc)
      hostConnector = Some(hc)
      context.watch(hc)
      retrieveWithHostConnector()

    case Terminated(_) =>
      log.debug("hostConnector terminated")
      hostConnector = None

    case HttpResponse(status, entity, _, _) =>
      val item = activeRequest.get
      val result = Try(parseAndCache(status, entity.asString))
      item.replyTo foreach { to => to ! result}
      activeRequest = None
      activeRequestStarted = 0L
      retrieveOrAskForHostConnector()

    case msg =>
      log.warning("Unknown message: {}", msg)
  }

  def retrievePriority(item: Retrievable[K]): Int = {
    item.replyTo match {
      case None => 3
      case Some(_) => 2
    }
  }

  def parseAndCache(status: StatusCode, body: String): V = {
    val item = activeRequest.get
    if (status.isSuccess) {
      val result = parser.parseBody(body)
      cache.put(item.key, result)
      log.debug("http get: {} ===> {} in {}ms", item.httpGetUri, status.intValue, System.currentTimeMillis - activeRequestStarted)
      result
    } else {
      log.debug("http get error: {} {} after {}ms", item.httpGetUri, status.intValue, System.currentTimeMillis() - activeRequestStarted)
      throw new IllegalStateException("http result not ok: " + status.intValue)
    }
  }

  def answerCachedOrElse(item: Retrievable[K], func: (Retrievable[K]) => Unit): Unit = {
    cache.get(item.key) match {
      case None =>
        func(item)
      case Some(result) =>
        if (result.isFresh) {
          item.replyTo.foreach { to => to ! result }
        } else {
          func(item)
        }
    }
  }

  def queueRetrieve(item: Retrievable[K]): Unit = {
    log.debug("queueRetrieve: {} {}", item)
    queue.enqueue(retrievePriority(item))(item)
    retrieveOrAskForHostConnector()
  }

  def retrieveOrAskForHostConnector(): Unit = {
    if (activeRequest.isEmpty) {
      log.debug("retrieveOrAskForHostConnector")
      hostConnector match {
        case None =>
          askForHostConnector()
        case Some(hc) =>
          if (hc == context.system.deadLetters) {
            askForHostConnector()
          } else {
            retrieveWithHostConnector()
          }
      }
    } else {
      log.debug("retrieveOrAskForHostConnector: retrieve already active")
    }
  }

  def askForHostConnector(): Unit = {
    log.debug("askForHostConnector")
    IO(Http) ! hostConnectorSetup
  }

  def retrieveWithHostConnector(): Unit = {
    log.debug("haveHcWillRetrieve")
    activeRequest match {
      case None =>
        queue.dequeueOption match {
          case None =>
            log.debug("queue is empty ...")
          case Some(item) =>
            answerCachedOrElse(item, sendToHostConnector)
        }
      case Some(_) =>
        log.debug("request already active, not doing anything")
    }
  }

  def sendToHostConnector(item: Retrievable[K]): Unit = {
    log.debug("send to hostConnector: {}:", item)
    hostConnector foreach { hc => hc ! HttpRequest(GET, item.httpGetUri) }
    activeRequest = Some(item)
    activeRequestStarted = System.currentTimeMillis()
  }

  def cachedAndNeedToRefresh(ids: Vector[(K, Uri)]): (Map[K, V], Vector[(K, Uri)], Vector[(K, Uri)]) = {
    val cached: Map[K, V] = Map() ++
      ids.map(pair => (pair._1, cache.get(pair._1)))
        .collect { case (k, Some(v)) => (k, v) }
    val uncached = ids.filterNot(pair => cached.contains(pair._1))
    val stale = ids.filter(pair => cached.contains(pair._1) && ! cached(pair._1).isFresh)
    (cached, uncached, stale)
  }

}
