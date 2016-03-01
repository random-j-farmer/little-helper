package me.rjfarmer.rlh.eve

import java.util.{Calendar, TimeZone}

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import jawn.ast.{JValue, JParser}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.cache.EhcLongCache
import me.rjfarmer.rlh.retriever._
import me.rjfarmer.rlh.server.{RequestHeaderData, Boot}
import org.ehcache.CacheManager
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
    Retriever.props[Long, ZkStats](new EhcLongCache[ZkStats](ehCache), retrieveQueue,
      priorityConfig, ZkStatsBodyParser.parseBody, retrieveTimeout, hostConnectorSetup)
  }

  def zkStats(zkStatsRetriever: ActorRef, headerData: RequestHeaderData, ids: Vector[Long], askTimeout: Timeout): Future[Map[Long, ZkStats]] = {

    ask(zkStatsRetriever, ZkStatsRetriGroup(headerData, ids, None))(askTimeout)
      .asInstanceOf[Future[Map[Long, ZkStats]]]

  }

}

final case class ZkStatsRetrievable(key: Long, priority: Int, replyTo: ActorRef) extends Retrievable[Long] {

  private[this] val uriPath: String = "/api/stats"

  override def httpGetUri: Uri = Uri(path = Uri.Path(uriPath + "/characterID/" + key.toLong))

}

final case class ZkStatsRetriGroup (headerData: RequestHeaderData, items: Vector[Long], replyTo: Option[ActorRef]) extends RetriGroup[Long] {

  override def retrievable(k: Long, priority: Int, replyTo: ActorRef): Retrievable[Long] = {
    ZkStatsRetrievable(k, priority, replyTo)
  }

}

object ZkStatsBodyParser {

  object YearAndMonth extends Ordering[ZkMonthStats] {
    override def compare(x: ZkMonthStats, y: ZkMonthStats): Int = {
      val yComp = x.year compare y.year
      if (yComp == 0)
        x.month compare y.month
      else
        yComp
    }
  }

  import PimpedJValue._


  def parseBody(characterID: Long, json: String): ZkStats = {

    val tree = JParser.parseFromString(json).get

    val activeNode = tree.get("activepvp")

    val activePvP = ZkActivePvP(int(activeNode.recget("kills", "count")),
      int(activeNode.recget("regions", "count")),
      int(activeNode.recget("ships", "count")),
      int(activeNode.recget("systems", "count")))

    val months = tree.get("months")

    val zkMonths = (for {
      Pair(name, value) <- months.asMapOrEmpty
    } yield {
        ZkMonthStats(int(value.get("year")),
          int(value.get("month")),
          int(value.get("shipsLost")),
          double(value.get("pointsLost")),
          double(value.get("iskLost")),
          int(value.get("shipsDestroyed")),
          double(value.get("pointsDestroyed")),
          double(value.get("iskDestroyed"))
        )
      }).toSeq


    val info = tree.get("info")
    val zkInfo = ZkInfo(long(info.get("allianceID")),
      long(info.get("corporationID")),
      long(info.get("factionID")),
      info.get("id").getLong.getOrElse(characterID),
      long(info.get("killID")),
      info.get("name").getString.getOrElse(""))

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


  private def int(jv: JValue) = jv.getInt.getOrElse(0)
  private def long(jv: JValue) = jv.getLong.getOrElse(0L)
  private def double(jv: JValue) = jv.getDouble.getOrElse(0.0d)



}
