package me.rjfarmer.rlh.eve

import java.util.{Calendar, TimeZone}

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.retriever._
import me.rjfarmer.rlh.server.Boot
import org.ehcache.CacheManager
import org.json4s._
import org.json4s.jackson.JsonMethods._
import spray.can.Http
import spray.http.Uri

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration

object ZkStatsRetriever {

  private val hostConnectorSetup = Http.HostConnectorSetup("zkillboard.com", port=443, sslEncryption = true, defaultHeaders = Retriever.defaultHeaders)

  import Boot.priorityConfig
  private val retrieveQueue = RetrieveQueue[Long](priorityConfig)

  def props(cacheManager: CacheManager, retrieveTimeout: FiniteDuration): Props = {
    val ehCache = cacheManager.getCache("zkStatsCache", classOf[java.lang.Long], classOf[ZkStats])
    Retriever.props[Long, ZkStats](new EhcRetrieveLongCache[ZkStats](ehCache), retrieveQueue,
      priorityConfig, ZkStatsBodyParser, retrieveTimeout, hostConnectorSetup)
  }

  def zkStats(zkStatsRetriever: ActorRef, wsr: WebserviceRequest, ids: Vector[Long], askTimeout: Timeout): Future[Map[Long, ZkStats]] = {

    ask(zkStatsRetriever, ZkStatsRetriGroup(wsr, ids, None))(askTimeout)
      .asInstanceOf[Future[Map[Long, ZkStats]]]

  }

}

final case class ZkStatsRetrievable(key: Long, priority: Int, replyTo: ActorRef) extends Retrievable[Long] {

  private[this] val uriPath: String = "/api/stats"

  override def httpGetUri: Uri = Uri(path = Uri.Path(uriPath + "/characterID/" + key.toLong))

}

final case class ZkStatsRetriGroup (wsr: WebserviceRequest, items: Vector[Long], replyTo: Option[ActorRef]) extends RetriGroup[Long] {

  override def retrievable(k: Long, priority: Int, replyTo: ActorRef): Retrievable[Long] = {
    ZkStatsRetrievable(k, priority, replyTo)
  }

}

object ZkStatsBodyParser extends BodyParser[Long, ZkStats] {

  object YearAndMonth extends Ordering[ZkMonthStats] {
    override def compare(x: ZkMonthStats, y: ZkMonthStats): Int = {
      val yComp = x.year compare y.year
      if (yComp == 0)
        x.month compare y.month
      else
        yComp
    }
  }

  override def parseBody(characterID: Long, json: String): ZkStats = {

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
