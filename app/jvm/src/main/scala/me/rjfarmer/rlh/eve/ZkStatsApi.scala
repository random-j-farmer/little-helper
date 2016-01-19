package me.rjfarmer.rlh.eve

import java.util.{Calendar, TimeZone}

import akka.actor._
import akka.io.IO
import akka.pattern.ask
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.ZkStatsApi.{GroupedZkStatsRequest, GroupedZkStatsResponse, ZkStatsRequest, ZkStatsResponse}
import me.rjfarmer.rlh.server.Boot
import org.ehcache.{Cache, CacheManager}
import spray.can.Http
import spray.http.HttpMethods._
import spray.http.{HttpRequest, HttpResponse, Uri}

import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success, Try}



object ZkStatsApi {

  def props(cacheManager: CacheManager, zkRestApi: ActorRef): Props = {
    val cache = cacheManager.getCache("zkStatsCache", classOf[java.lang.Long], classOf[ZkStats])
    Props(new ZkStatsApi(cache, zkRestApi))
  }

  final case class GroupedZkStatsRequest(ids: Vector[Long], replyTo: Option[ActorRef])

  final case class GroupedZkStatsResponse(infoById: Map[Long, ZkStats])

  final case class ZkStatsRequest(characterID: Long, replyTo: Option[ActorRef], cacheTo: Option[ActorRef])

  final case class ZkStatsResponse(request: ZkStatsRequest, stats: Try[ZkStats])

}

/** In-Memory cached zkillboard stats */
class ZkStatsApi (cache: Cache[java.lang.Long, ZkStats], zkRestApi: ActorRef) extends Actor with ActorLogging {

  override def receive: Receive = {

    case GroupedZkStatsRequest(ids, None) =>
      // for easy asking
      self ! GroupedZkStatsRequest(ids, Some(sender()))

    case GroupedZkStatsRequest(ids, Some(replyTo)) =>
      val cached: Map[Long, ZkStats] = Map() ++
        ids.map(id => (id, cache.get(id)))
          .filter(pair => pair._2 != null)
      val uncachedIds = ids.filterNot(cached.contains)
      val stalest = cached.values
        .toVector
        .filterNot(_.isFresh)
        .sortWith((ci1, ci2) => ci1.receivedTimestamp < ci2.receivedTimestamp)
        .map(_.info.id)
        .take(10)
      val need = uncachedIds ++ stalest
      log.debug("grouped zkstats request: {} requested / {} uncached / {} refresh stale",
        ids.size, uncachedIds.size, stalest.size)
      if (need.isEmpty) {
        replyTo ! GroupedZkStatsResponse(cached)
      } else {
        sendGroupedResponse(replyTo, cached, need)
      }
    case ZkStatsResponse(req, tzs) =>
      tzs match {
        case Success(zs) =>
          log.debug("caching zkstats for {}", req.characterID)
          cache.put(req.characterID, zs)
        case Failure(ex) =>
          log.debug("not caching character info response: {}", ex)
      }

    case msg =>
      log.warning("unknown message type: {}", msg)
  }

  import Boot._
  implicit val timeoutDuration = bootTimeout.duration
  import scala.concurrent.ExecutionContext.Implicits.global

  private def zkStats(id: Long, cc: ActorRef): Future[ZkStats] = {
    ask(zkRestApi, ZkStatsRequest(id, None, Some(cc)))
      .asInstanceOf[Future[ZkStatsResponse]]
      .map(resp => resp.stats.get)
  }

  def sendGroupedResponse(replyTo: ActorRef, cached: Map[Long, ZkStats], need: Vector[Long]): Unit = {
    Future.sequence(need.map(id => zkStats(id, self)))
      .onComplete {
        case Success(cis) =>
          val result = cached ++ cis.map(zk => (zk.info.id, zk))
          replyTo ! GroupedZkStatsResponse(result)
        case Failure(ex) =>
          log.error("sendGroupedResponse: using cached (stale?) response because we received an error: {}", ex)
          // accessing cache is safe - its a concurrent cache
          // some retrieves might have worked and are in the cache now
          val result = cached ++ need.map(id => (id, cache.get(id))).filter(pair => pair._2 != null)
          replyTo ! GroupedZkStatsResponse(result)
      }
  }

}


/*

 I would have loved to use upickle, but the input json is too flexible.
 e.g. activepvp may be an empty list (if the player is not active in pvp) or it may be an object
 with counters.

 upickle is all about type safety and does not like this.

 */


object RestZkStatsApi {

  def props: Props = Props[RestZkStatsApi]()

}

/** Rest ZKillboard Stats */
class RestZkStatsApi extends Actor with ActorLogging {

  import scala.concurrent.ExecutionContext.Implicits.global
  implicit val timeout = Boot.bootTimeout

  override def receive: Receive = {

    case ZkStatsRequest(id, None, cacheTo) =>
      // for easy asking
      self ! ZkStatsRequest(id, Some(sender()), cacheTo)
      CharacterInfo
    case request @ ZkStatsRequest(id, Some(replyTo), cacheTo) =>
      val fzs = complete(id)
      fzs.onComplete { tci =>
        val resp = ZkStatsResponse(request, tci)
        replyTo ! resp
        cacheTo.foreach(cc => cc ! resp)
      }

    case msg =>
      log.warning("unknown message type: {}", msg)

  }

  def uriPath: String = "/api/stats"

  def hostConnector: ActorRef = {
    implicit val actorSystem = context.system
    Await.result(ask(IO(Http),hostConnectorSetup), timeout.duration)
      .asInstanceOf[Http.HostConnectorInfo]
      .hostConnector
  }

  def hostConnectorSetup = Http.HostConnectorSetup("zkillboard.com", port=443, sslEncryption = true)

  def httpGetUri(characterID: Long): Uri = Uri(path = Uri.Path(uriPath + "/characterID/" + characterID))

  def complete(characterID: Long): Future[ZkStats] = {
    val started = System.currentTimeMillis()
    val uri = httpGetUri(characterID)
    log.debug("http get: {}", uri)
    val httpFuture = ask(hostConnector, HttpRequest(GET, httpGetUri(characterID)))
    val promise = Promise[ZkStats]()
    httpFuture onSuccess {
      case resp: HttpResponse =>
        if (resp.status.isSuccess) {
          log.debug("http get: {} ===> {} in {}ms", uri, resp.status.intValue, System.currentTimeMillis() - started)
          promise.complete(Try(successMessage(characterID, resp.entity.data.asString)))
        } else {
          log.debug("http get error: {} {} after {}ms", resp.status.intValue, resp.entity.data.asString,
            System.currentTimeMillis() - started)
          promise.failure(new IllegalArgumentException("http result not ok: " + resp.status.intValue))
        }
    }
    promise.future
  }

  import org.json4s._
  import org.json4s.jackson.JsonMethods._

  object YearAndMonth extends Ordering[ZkMonthStats] {
    override def compare(x: ZkMonthStats, y: ZkMonthStats): Int = {
      val yComp = x.year compare y.year
      if (yComp == 0)
        x.month compare y.month
      else
        yComp
    }
  }

  def successMessage(characterID: Long, json: String): ZkStats = {

    val tree = parse(json)

    val activeNode = tree \ "activepvp"

    val activePvP = ZkActivePvP(int(activeNode \ "kills" \ "count") getOrElse 0,
      int(activeNode \ "regions" \ "count") getOrElse 0,
      int(activeNode \ "ships" \ "count") getOrElse 0,
      int(activeNode \ "systems" \ "count") getOrElse 0)

    val months = tree \ "months"

    val zkMonths = (for {
      Pair(name, value) <- obj(months).getOrElse(Seq())
      vmap = value.asInstanceOf[Map[String, Any]]
    } yield {
        ZkMonthStats(doubleOrZero(vmap, "year").toInt,
          doubleOrZero(vmap, "month").toInt,
          doubleOrZero(vmap, "shipsLost").toInt,
          doubleOrZero(vmap, "pointsLost"),
          doubleOrZero(vmap, "iskLost"),
          doubleOrZero(vmap, "shipsDestroyed").toInt,
          doubleOrZero(vmap, "pointsDestroyed"),
          doubleOrZero(vmap, "iskDestroyed")
        )
      }).toSeq


    val info = tree \ "info"
    val zkInfo = ZkInfo(long(info \ "allianceID").getOrElse(0L),
      long(info \ "corporationID").getOrElse(0L),
      long(info \ "factionID").getOrElse(0),
      long(info \ "id").getOrElse(characterID),
      long(info \ "killID").getOrElse(0L),
      str(info \ "name").getOrElse(""))

    ZkStats(zkInfo, activePvP, lastMonths(zkMonths), System.currentTimeMillis())

  }


  /** sum of stats for last 2 months (i.e. current month and the one before) */
  def lastMonths(months: Seq[ZkMonthStats]): ZkMonthStats = {
    val cal = Calendar.getInstance()
    cal.setTimeZone(TimeZone.getTimeZone("UTC"))
    val thisMonth = cal.get(Calendar.YEAR) -> (cal.get(Calendar.MONTH) + 1)
    cal.add(Calendar.MONTH, -1)
    val lastMonth = cal.get(Calendar.YEAR) -> (cal.get(Calendar.MONTH) + 1)

    val filtered = months filter { m =>
      (m.year == lastMonth._1 && m.month == lastMonth._2) ||
        (m.year == thisMonth._1 && m.month == thisMonth._2)
    }

    ZkMonthStats(lastMonth._1, lastMonth._2,
      filtered.map(_.shipsLost).sum,
      filtered.map(_.pointsLost).sum,
      filtered.map(_.iskLost).sum,
      filtered.map(_.shipsDestroyed).sum,
      filtered.map(_.pointsDestroyed).sum,
      filtered.map(_.iskDestroyed).sum)
  }



  //
  // helper methods because unapply does not seem to work for json4s/jackson
  //
  def str(elem: JValue): Option[String] = {
    elem match {
      case s:JString => Option(s.values)
      case _ => None
    }
  }

  def int(elem: JValue): Option[Int] = {
    long(elem).map(_.toInt)
  }

  def long(elem: JValue): Option[Long] = {
    elem match {
      case s:JInt => Option(s.values.toLong)
      case s:JLong => Option(s.values)
      case _ => None
    }
  }

  def double(elem: JValue): Option[Double] = {
    elem match {
      case s:JDouble => Option(s.values)
      case s:JDecimal => Option(s.values.toDouble)
      case s:JInt => Option(s.values.toDouble)
      case s:JLong => Option(s.values.toDouble)
      case _ => None
    }
  }

  def doubleOrZero(elem: JValue): Double = {
    double(elem).getOrElse(0.0d)
  }

  def double(m: Map[String, Any], s: String): Option[Double] = {
    val v = m.get(s)
    v match {
      case Some(value) => value match {
        case d: Double => Some(d)
        case bd: BigDecimal => Some(bd.toDouble)
        case l: Long => Some(l.toDouble)
        case i: Int => Some(i.toDouble)
        case bi: BigInt => Some(bi.toDouble)
        case _ => None
      }
      case None => None
    }

  }

  def doubleOrZero(m: Map[String, Any], s: String): Double = {
    double(m, s).getOrElse(0.0d)
  }


  def obj(elem: JValue): Option[Map[String, Any]] = {
    elem match {
      case o:JObject => Some(o.values)
      case _ => None
    }
  }

}

