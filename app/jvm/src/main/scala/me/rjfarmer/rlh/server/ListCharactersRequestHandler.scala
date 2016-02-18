package me.rjfarmer.rlh.server

import akka.pattern.ask
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.CharacterIDApi.{CharacterIDRequest, CharacterIDResponse}
import me.rjfarmer.rlh.eve.{CharacterInfoRetriever, ZkStatsRetriever}
import org.ehcache.Cache

import scala.concurrent.Future

/**
 * list characters request handler
 *
 * @param cache response cache
 */
class ListCharactersRequestHandler(val cache: ResponseCache[ListCharactersResponse])
  extends CachingRequestHandler[ListCharactersRequest, ListCharactersResponse, ListCharactersResponse] {

  import Boot._

  import scala.concurrent.ExecutionContext.Implicits.global

  override def clientVersionError(req: ListCharactersRequest): ListCharactersResponse = {
    ListCharactersResponse(Some(Server.clientVersionError), None, req.solarSystem, Vector())
  }

  private def listIds(wsr: WebserviceRequest, names: Vector[String]) = {
    ask(Server.characterID, CharacterIDRequest(wsr, names, Map(), None))(Boot.ajaxFutureTimeout)
      .asInstanceOf[Future[CharacterIDResponse]]
  }

  final case class Holder(idResp: CharacterIDResponse, infoMap: Map[Long, CharacterInfo], zkMap: Map[Long, ZkStats])

  private def charinfoAndZkStats(req: ListCharactersRequest, ts: Long)(idResp: CharacterIDResponse) = {
    val pureIds = idResp.fullResult.values.map(_.characterID).toVector
    if (idResp.unknownNames.nonEmpty) {
      bootSystem.log.warning("<{}> unknown character names: {}", req.clientIP, idResp.unknownNames.mkString(", "))
    }
    val f1 = CharacterInfoRetriever.characterInfo(Server.characterInfoRetriever, req, pureIds, ajaxFutureTimeout)
    val f2 = ZkStatsRetriever.zkStats(Server.zkStatsRetriever, req, pureIds, ajaxFutureTimeout)
    f1.zip(f2).map(pair => Holder(idResp, pair._1, pair._2))
  }

  private def combineResults(req: ListCharactersRequest, ts: Long)(h: Holder) = {
    val cis = h.idResp.allNames.map { name =>
      val idOpt = h.idResp.fullResult.get(name).map(ian => ian.characterID)
      val id: Long = idOpt.getOrElse(0L)
      // no results for key 0L, so if the id was not resolved, we return None
      CharInfo(name, idOpt, h.infoMap.get(id), h.zkMap.get(id))
    }.sorted(ByDestroyed)
    bootSystem.log.info("<{}> listCharacters: successful response for {} names ({} stale) in {}ms",
      req.clientIP, req.names.size,
      cis.filterNot(_.isFresh).length,
      System.currentTimeMillis() - ts)
    ListCharactersResponse(None, None, req.solarSystem, cis)
  }

  override def handleUncached(req: ListCharactersRequest): Future[ListCharactersResponse] = {
    val ts = System.currentTimeMillis()
    val idsFuture = listIds(req, req.names)
    idsFuture.flatMap(charinfoAndZkStats(req, ts))
      .map(combineResults(req, ts))
      .fallbackTo(Future.successful(ListCharactersResponse(Some("Error parsing request lines"), None, req.solarSystem, Vector())))
  }

}

object ListCharactersRequestHandler {

  def apply(cache: ResponseCache[ListCharactersResponse]): ListCharactersRequestHandler = new ListCharactersRequestHandler(cache)

}

// list characters response cache helper
final case class ListCharactersResponseCache(cache: Cache[String, ListCharactersResponse]) extends ResponseCache[ListCharactersResponse]
