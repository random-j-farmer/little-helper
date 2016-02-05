package me.rjfarmer.rlh.retriever

import akka.actor._
import akka.io.IO
import me.rjfarmer.rlh.api.{WebserviceRequest, WebserviceResult}
import me.rjfarmer.rlh.server.Boot
import org.ehcache.Cache
import spray.can.Http
import spray.can.Http.{HostConnectorInfo, HostConnectorSetup}
import spray.http.HttpHeaders.RawHeader
import spray.http.HttpMethods.GET
import spray.http._
import spray.httpx.encoding.{Deflate, Gzip}

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

  def priority: Int

  def replyTo: ActorRef

}

/**
 * A groupp of retrievables.
 *
 * replyTo is an option for asking convenience (i.e. send in None, it will be answered to the sender)
 *
 * @tparam K key of retrievables
 */
trait RetriGroup[K] {

  def items: Vector[K]

  def wsr: WebserviceRequest

  def replyTo: Option[ActorRef]

  def retrievable(k: K, priority: Int, replyTo: ActorRef): Retrievable[K]

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
class EhcRetrieveLongCache[V <: WebserviceResult] (cache: Cache[java.lang.Long, V]) extends RetrieveCache[Long, V] {

  def get(k: Long): Option[V] = Option(cache.get(k))

  def put(k: Long, v: V): Unit = cache.put(k, v)

}


/**
 * Parses response bodies for retriever.
 *
 * @tparam K key type
 * @tparam V value type
 */
trait BodyParser[K, V] {

  def parseBody(key: K, body: String): V

}

/** Helper trait for response body decoding */
trait ResponseBodyDecoder {

  def decodeResponseBody(resp: HttpResponse): String = {
    resp.encoding match {
      case HttpEncoding("gzip") =>
        Gzip.decode(resp).entity.asString
      case HttpEncoding("deflate") =>
        Deflate.decode(resp).entity.asString
      case _ =>
        resp.entity.asString
    }
  }

}


object Retriever {

  def props[K, V <: WebserviceResult](cache: RetrieveCache[K, V],
                                      queue: RetrieveQueue[K],
                                      prioConf: PriorityConfig,
                                      parser: BodyParser[K, V],
                                      timeout: FiniteDuration,
                                      hostConnectorSetup: HostConnectorSetup): Props =
    Props(new Retriever[K, V](cache, queue, prioConf, parser, timeout, hostConnectorSetup))


  val defaultHeaders: List[HttpHeader] =  if (Boot.bootConfig.getBoolean("little-helper.xml-api.use-compression")) {
    List(RawHeader("accept-encoding", "gzip,deflate"))
  } else {
    List()
  }

}

/**
 * Retriever retrieves stuff by http get.
 *
 * It uses an internal priority queue to queue requests, only one request
 * is active per retriever.
 *
 * This lets us complete background requests with low priority while
 * still completing important requests fast.
 *
 * You probably want to pool them.  Ideally
 * the pool has the same size as the number of connections per host.
 * This will let CharacterID requests be processed immediately
 * (because they do not use retriever, so they are not queued).
 * Which is important, because CharacterIDRequests are the most
 * important ones.
 *
 * @param cache concurrent cache used by all pooled retrievers (and collectors)
 * @param queue concurrent priority queue shared by pooled retrievers
 * @param prioConf priority configuration  for use with queue
 * @param parser parser for incoming response bodies
 * @param timeout timeout for completion, incomplete response if we go over
 * @param hostConnectorSetup host connector setup
 * @tparam K key type, ususually Long (characterID)
 * @tparam V value type, e.g. CharacterInfo or ZkStats
 */
class Retriever[K, V <: WebserviceResult](cache: RetrieveCache[K, V],
                                          queue: RetrieveQueue[K],
                                          prioConf: PriorityConfig,
                                          parser: BodyParser[K, V],
                                          timeout: FiniteDuration,
                                          hostConnectorSetup: HostConnectorSetup)
  extends Actor with ActorLogging with ResponseBodyDecoder {

  import me.rjfarmer.rlh.server.Boot.bootSystem

  private[this] var hostConnector: Option[ActorRef] = None
  private[this] var activeRequest: Option[Retrievable[K]] = None
  private[this] var activeRequestStarted: Long = 0L

  override def receive: Receive = {

    case group: RetriGroup[K] =>
      handleRetriGroup(group)

    case item : Retrievable[K] =>
      // log.debug("Retrievable: {}", item)
      answerCachedOrElse(item, queueRetrieve)

    case HostConnectorInfo(hc, _) =>
      // log.debug("hostConnector {}", hc)
      hostConnector = Some(hc)
      context.watch(hc)
      retrieveWithHostConnector()

    case Terminated(_) =>
      log.debug("hostConnector terminated")
      hostConnector = None

    case resp: HttpResponse =>
      // log.debug("HttpResponse: {}", resp.status)
      val item = activeRequest.get
      val result = Try(parseAndCache(resp))
      if (item.replyTo != context.system.deadLetters) {
        item.replyTo ! Collector.Result(item, result)
      }
      activeRequest = None
      activeRequestStarted = 0L
      retrieveOrAskForHostConnector()

    case msg =>
      log.warning("Unknown message: {}", msg)
  }

  def parseAndCache(resp: HttpResponse): V = {
    val status = resp.status
    val item = activeRequest.get
    if (status.isSuccess) {
      try {
        val result = parser.parseBody(item.key, decodeResponseBody(resp))
        cache.put(item.key, result)
        log.debug("http get p{}: {} ===> {} in {}ms", item.priority, item.httpGetUri, status.intValue,
          System.currentTimeMillis - activeRequestStarted)
        result
      } catch {
        case e: Exception =>
          log.error("error parsing response body: {}", e)
          throw e
      }
    } else {
      log.debug("http get error: {} {} after {}ms", item.httpGetUri, status.intValue,
        System.currentTimeMillis() - activeRequestStarted)
      throw new IllegalStateException("http result not ok: " + status.intValue)
    }
  }

  def answerCachedOrElse(item: Retrievable[K], func: (Retrievable[K]) => Unit): Unit = {
    cache.get(item.key) match {
      case None =>
        func(item)
      case Some(result) =>
        if (result.isFresh) {
          item.replyTo ! result
        } else {
          func(item)
        }
    }
  }

  def queueRetrieve(item: Retrievable[K]): Unit = {
    // log.debug("queueRetrieve: {} {}", item)
    queue.enqueue(item)
    retrieveOrAskForHostConnector()
  }

  def retrieveOrAskForHostConnector(): Unit = {
    if (activeRequest.isEmpty) {
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
    }
  }

  def askForHostConnector(): Unit = {
    IO(Http) ! hostConnectorSetup
  }

  def retrieveWithHostConnector(): Unit = {
    activeRequest match {
      case None =>
        queue.dequeueOption match {
          case None =>
          case Some(item) =>
            answerCachedOrElse(item, sendToHostConnector)
        }
      case Some(_) =>
        // log.debug("request already active, not doing anything")
    }
  }

  def sendToHostConnector(item: Retrievable[K]): Unit = {
    hostConnector foreach { hc => hc ! HttpRequest(GET, item.httpGetUri) }
    activeRequest = Some(item)
    activeRequestStarted = System.currentTimeMillis()
  }

  def handleRetriGroup(group: RetriGroup[K]): Unit = {
    val cached: Map[K, V] = Map() ++
      group.items.map{ id => (id, cache.get(id))}
        .collect { case (k, Some(v)) => (k, v) }
    val uncached = group.items.filterNot(id => cached.contains(id))

    val stale = cached
      .filterNot(_._2.isFresh)
      .toVector
      .sortWith((p1, p2) => p1._2.receivedTimestamp < p2._2.receivedTimestamp)
      .map(_._1)

    val highPrio = prioConf.priority(uncached.length)
    val numPromoted = Math.min(prioConf.promote(uncached.length), stale.length)
    val highPrioItems = uncached ++ stale.take(numPromoted)

    val stalePrioItems = stale.drop(numPromoted)
    val stalePrio = Math.max(highPrio, prioConf.priority(stalePrioItems.length)) + prioConf.stalePriorityOffset

    log.info(s"<${group.wsr.clientIP}> grouped request: ${group.items.size} total / ${cached.size} cached (${stale.size} stale) " +
      s"/ p$highPrio:${uncached.length + numPromoted} p$stalePrio:${stalePrioItems.length}")

    val replyTo = group.replyTo match {
      case None => sender()
      case Some(x) => x
    }

    val totalItems = highPrioItems.length + stalePrioItems.length
    if (totalItems == 0) {
      replyTo ! cached
    } else {
      val collector = context.actorOf(Collector.props(cache, cached, highPrioItems.length, replyTo, timeout))
      highPrioItems.foreach { key =>
        self ! group.retrievable(key, highPrio, collector)
      }
      stalePrioItems.foreach { key =>
        self ! group.retrievable(key, stalePrio, collector)
      }
    }

  }

}
