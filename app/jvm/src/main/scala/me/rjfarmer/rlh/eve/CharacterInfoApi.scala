package me.rjfarmer.rlh.eve

import akka.actor._
import akka.pattern.ask
import me.rjfarmer.rlh.api.{WebserviceResult, CharacterInfo, EmploymentHistory}
import me.rjfarmer.rlh.eve.CharacterInfoApi.{CharacterInfoRequest, CharacterInfoResponse, GroupedCharacterInfoRequest, GroupedCharacterInfoResponse}
import me.rjfarmer.rlh.eve.EveCharacterInfoApi.CharacterInfoXml
import me.rjfarmer.rlh.server.Boot
import org.ehcache.{Cache, CacheManager}
import spray.http.Uri

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import scala.xml.XML

object CharacterInfoApi {

  def props(cacheManager: CacheManager, eveCharacterInfo: ActorRef): Props = {
    val cache = cacheManager.getCache("characterInfoCache", classOf[java.lang.Long], classOf[CharacterInfo])
    Props(new CharacterInfoApi(cache, eveCharacterInfo))
  }

  final case class GroupedCharacterInfoRequest(ids: Vector[Long], replyTo: Option[ActorRef])

  final case class GroupedCharacterInfoResponse(infoById: Map[Long, CharacterInfo])

  final case class CharacterInfoRequest(characterID: Long, replyTo: Option[ActorRef], cacheTo: Option[ActorRef])

  final case class CharacterInfoResponse(request: CharacterInfoRequest, result: Try[CharacterInfo])

}

/** logic to decide what to fetch */
trait CacheRefresher[T <: WebserviceResult] {

  def cache: Cache[java.lang.Long, T]

  def characterID(wsr: T): Long

  def minRefreshStale: Int

  def cachedAndNeedToRefresh(ids: Vector[Long]): (Map[Long, T], Vector[Long]) = {
    val cached: Map[Long, T] = Map() ++
      ids.map(id => (id, cache.get(id)))
        .filter(pair => pair._2 != null)
    val uncachedIds = ids.filterNot(cached.contains)
    val refreshNum = math.max(minRefreshStale, (ids.length * 0.1d).toInt - uncachedIds.length)
    val stalest = cached.values
      .toVector
      .filterNot(_.isFresh)
      .sortWith((ci1, ci2) => ci1.receivedTimestamp < ci2.receivedTimestamp)
      .map(characterID)
      .take(refreshNum)
    val need = uncachedIds ++ stalest
    (cached, need)
  }

}

/**
 * Main character info api.
 *
 * This only does the caching, the real work is sent on to EveCharacterInfoApi actors.
 *
 * cache usage is done this way because the easy api also cache unsuccesful results.
 *
 * @param cache lru cache
 * @param eveCharacterInfo actorref for eve xml api
 */
class CharacterInfoApi (val cache: Cache[java.lang.Long, CharacterInfo], eveCharacterInfo: ActorRef)
  extends Actor with ActorLogging with CacheRefresher[CharacterInfo] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def characterID(ci: CharacterInfo) = ci.characterID

  override val minRefreshStale = Boot.minRefreshStale

  override def receive: Receive = {


    case GroupedCharacterInfoRequest(ids, None) =>
      // for easy asking
      self ! GroupedCharacterInfoRequest(ids, Some(sender()))

    case GroupedCharacterInfoRequest(ids, Some(replyTo)) =>
      val (cached, need) = cachedAndNeedToRefresh(ids)
      log.info("grouped character info request: {} total / {} cached / {} need to refresh",
        ids.size, cached.size, need.size)
      if (need.isEmpty) {
        replyTo ! GroupedCharacterInfoResponse(cached)
      } else {
        sendGroupedResponse(replyTo, cached, need)
      }

    case CharacterInfoResponse(req, tci) =>
      tci match {
        case Success(ci) =>
          log.debug("caching character info for {}", req.characterID)
          cache.put(req.characterID, ci)
        case Failure(ex) =>
          log.debug("not caching character info response: {}", ex)
      }

    case msg =>
      log.warning("unknown message type: {}", msg)

  }

  import Boot._
  implicit val timeoutDuration = bootTimeout.duration

  private def characterInfo(id: Long, cc: ActorRef): Future[CharacterInfo] = {
    ask(eveCharacterInfo, CharacterInfoRequest(id, None, Some(cc)))
      .asInstanceOf[Future[CharacterInfoResponse]]
      .map(resp => resp.result.get)
  }

  def sendGroupedResponse(replyTo: ActorRef, cached: Map[Long, CharacterInfo], need: Vector[Long]): Unit = {
    Future.sequence(need.map(id => characterInfo(id, self)))
      .onComplete {
        case Success(cis) =>
          val result = cached ++ cis.map(ci => (ci.characterID, ci))
          replyTo ! GroupedCharacterInfoResponse(result)
        case Failure(ex) =>
          log.error("sendGroupedResponse: using cached (stale?) response because we received an error: {}", ex)
          // accessing cache is safe - its a concurrent cache
          // some retrieves might have worked and are in the cache now
          val result = cached ++ need.map(id => (id, cache.get(id))).filter(pair => pair._2 != null)
          replyTo ! GroupedCharacterInfoResponse(result)
      }
  }

}

object EveCharacterInfoApi {

  def props: Props = Props[EveCharacterInfoApi]()

  /** for testing the parsing part */
  final case class CharacterInfoXml(xml: String)

}

/**
 * Eve XML Character Info
 */
class EveCharacterInfoApi extends Actor with ActorLogging with EveXmlApi[CharacterInfo] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def receive: Receive = {

    case CharacterInfoRequest(id, None, cacheTo) =>
      // for easy asking
      self ! CharacterInfoRequest(id, Some(sender()), cacheTo)

    case request@CharacterInfoRequest(id, Some(replyTo), cacheTo) =>
      val fci: Future[CharacterInfo] = complete(Uri.Query(Pair("characterID", id.toString)))
      fci.onComplete { tci =>
        val resp = CharacterInfoResponse(request, tci)
        replyTo ! resp
        cacheTo.foreach(cc => cc ! resp)
      }

    case CharacterInfoXml(xml) =>
      // for testing the xml parsing
      sender() ! successMessage(xml)

    case msg =>
      log.warning("unknown message type: {}", msg)

  }

  val uriPath = "/eve/CharacterInfo.xml.aspx"

  def successMessage(xml: String): CharacterInfo = {
    // log.debug("xml: {}", xml)
    val elem = (XML.loadString(xml) \\ "result")(0)

    val eh = for {
      history <- elem \ "rowset" if history \@ "name" == "employmentHistory"
      row <- history \ "row"
    } yield EmploymentHistory(row \@ "corporationID", row \@ "corporationName", row \@ "startDate")

    val firstEmployment = parseDatetime(if (eh.isEmpty) etxt(elem, "corporationDate") else eh.last.startDate).getTime
    val now = System.currentTimeMillis()
    val age = (now - firstEmployment) / (1000.0d * 3600.0d * 24.0d * 365.242199d)

    CharacterInfo(etxt(elem, "characterID").toLong,
      etxt(elem, "characterName"),
      etxt(elem, "race"), etxt(elem, "bloodline"), etxt(elem, "ancestry"),
      etxt(elem, "corporationID").toLong, etxt(elem, "corporation"), etxt(elem, "corporationDate"),
      opttxt(elem, "allianceID").map(_.toLong), opttxt(elem, "alliance"), opttxt(elem, "allianceDate"),
      etxt(elem, "securityStatus").toDouble, age, eh,
      System.currentTimeMillis())
  }

}



