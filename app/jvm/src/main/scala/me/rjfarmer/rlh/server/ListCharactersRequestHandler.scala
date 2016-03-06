package me.rjfarmer.rlh.server

import akka.pattern.ask
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.cache.{EhcCache, EhcStringCache}
import me.rjfarmer.rlh.eve.CharacterIDApi.{CharacterIDRequest, CharacterIDResponse}
import me.rjfarmer.rlh.eve.{CharacterInfoRetriever, ZkStatsRetriever}
import org.ehcache.Cache

import scala.concurrent.Future

/**
 * list characters request handler
 *
 * @param cache response cache
 */
class ListCharactersRequestHandler(val cache: EhcCache[String, ListCharactersResponse])
  extends CachingRequestHandler[ListCharactersRequest, ListCharactersResponse] {

  import Boot._

  import scala.concurrent.ExecutionContext.Implicits.global

  override def clientVersionError(headerData: RequestHeaderData, req: ListCharactersRequest): ListCharactersResponse = {
    ListCharactersResponse(Some(Server.clientVersionError), None, headerData.solarSystem,
      headerData.refreshedJsonWebToken, System.currentTimeMillis(), Vector())
  }

  private def listIds(headerData: RequestHeaderData, names: Vector[String]) = {
    ask(Server.characterID, CharacterIDRequest(headerData, names, Map(), None))(Boot.ajaxFutureTimeout)
      .asInstanceOf[Future[CharacterIDResponse]]
  }

  final case class Holder(idResp: CharacterIDResponse, infoMap: Map[Long, CharacterInfo], zkMap: Map[Long, ZkStats])

  private def charinfoAndZkStats(headerData: RequestHeaderData, req: ListCharactersRequest, ts: Long)(idResp: CharacterIDResponse) = {
    val pureIds = idResp.fullResult.values.map(_.characterID).toVector
    if (idResp.unknownNames.nonEmpty) {
      bootSystem.log.warning("<{}> unknown character names: {}", headerData.clientIP, idResp.unknownNames.mkString(", "))
    }
    val f1 = CharacterInfoRetriever.characterInfo(Server.characterInfoRetriever, headerData, pureIds, ajaxFutureTimeout)
    val f2 = ZkStatsRetriever.zkStats(Server.zkStatsRetriever, headerData, pureIds, ajaxFutureTimeout)
    f1.zip(f2).map(pair => Holder(idResp, pair._1, pair._2))
  }

  object ByDestroyed extends Ordering[CharInfo] {
    override def compare(x: CharInfo, y: CharInfo): Int = {
      val yi = y.recentKills.getOrElse(0)
      val xi = x.recentKills.getOrElse(0)
      val ya = y.characterAge.getOrElse(-1.0d)
      val xa = x.characterAge.getOrElse(-1.0d)
      val compKilled = yi.compareTo(xi)
      if (compKilled != 0) compKilled else ya.compareTo(xa)
    }
  }

  private def combineResults(headerData: RequestHeaderData, req: ListCharactersRequest, ts: Long)(h: Holder) = {
    val cis = h.idResp.allNames.map { name =>
      val idOpt = h.idResp.fullResult.get(name).map(ian => ian.characterID)
      val id: Long = idOpt.getOrElse(0L)
      // no results for key 0L, so if the id was not resolved, we return None
      CharInfo(name, idOpt, h.infoMap.get(id), h.zkMap.get(id))
    }.sorted(ByDestroyed)
    bootSystem.log.info("<{}> listCharacters: successful response for {} names ({} stale) in {}ms",
      headerData.clientIP, req.names.size,
      cis.filterNot(_.isFresh).length,
      System.currentTimeMillis() - ts)
    ListCharactersResponse(None, None, headerData.solarSystem, headerData.refreshedJsonWebToken, System.currentTimeMillis(), cis)
  }

  override def handleUncached(headerData: RequestHeaderData, req: ListCharactersRequest): Future[ListCharactersResponse] = {
    val ts = System.currentTimeMillis()
    val idsFuture = listIds(headerData, req.names)
    idsFuture.flatMap(charinfoAndZkStats(headerData, req, ts))
      .map(combineResults(headerData, req, ts))
      .fallbackTo(Future.successful(ListCharactersResponse(Some("Error parsing request lines"), None, headerData.solarSystem,
        headerData.refreshedJsonWebToken, System.currentTimeMillis(), Vector())))
  }

  /** abstract method that will produce a response with cachekey added */
  override def copyWithCacheKey(key: String, resp: ListCharactersResponse): ListCharactersResponse = resp.copy(cacheKey = Some(key))
}

object ListCharactersRequestHandler {

  def apply(cache: EhcCache[String, ListCharactersResponse]): ListCharactersRequestHandler = new ListCharactersRequestHandler(cache)

}

// list characters response cache helper
class ListCharactersResponseCache(cache: Cache[String, ListCharactersResponse]) extends EhcStringCache[ListCharactersResponse](cache)
