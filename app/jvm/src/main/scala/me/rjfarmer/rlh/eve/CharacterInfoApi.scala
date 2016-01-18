package me.rjfarmer.rlh.eve

import akka.actor._
import akka.pattern.ask
import akka.routing.FromConfig
import me.rjfarmer.rlh.api.{CharacterInfo, EmploymentHistory}
import me.rjfarmer.rlh.eve.CharacterInfoApi.{CharacterInfoRequest, CharacterInfoResponse}
import me.rjfarmer.rlh.server.Boot
import org.ehcache.{Cache, CacheManager}
import spray.http.Uri

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.XML

object CharacterInfoApi {

  def props(cacheManager: CacheManager, eveCharacterInfo: ActorRef): Props = {
    val cache = cacheManager.getCache("characterInfoCache", classOf[java.lang.Long], classOf[CharacterInfo])
    Props(new CharacterInfoApi(cache, eveCharacterInfo))
  }

  final case class CharacterInfoRequest(characterID: Long, replyTo: Seq[ActorRef])

  final case class CharacterInfoResponse(request: CharacterInfoRequest, result: Try[CharacterInfo])


  def main(args: Array[String]) = {
    import Boot._

    try {
      val eveCharacterInfo = bootSystem.actorOf(FromConfig.props(EveCharacterInfoApi.props), "eveCharacterInfoPool")
      val characterInfo = bootSystem.actorOf(FromConfig.props(CharacterInfoApi.props(cacheManager, eveCharacterInfo)), "characterInfoPool")

      println("ARG: " + args(0))

      val rslt = Await.result(ask(characterInfo, CharacterInfoRequest(args(0).toLong, Seq())), bootTimeout.duration).asInstanceOf[CharacterInfoResponse]
      println("RESULT: " + rslt)
    } finally {
      bootSystem.shutdown()
    }
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
class CharacterInfoApi (cache: Cache[java.lang.Long, CharacterInfo], eveCharacterInfo: ActorRef)
  extends Actor with ActorLogging {


  override def receive: Receive = {

    case CharacterInfoRequest(id, Seq()) =>
      // for convenience of testing via ask
      self ! CharacterInfoRequest(id, Seq(sender()))

    case request@CharacterInfoRequest(id, replyTo) =>
      val ci = cache.get(id)
      if (ci == null) {
        eveCharacterInfo ! request.copy(replyTo = replyTo :+ self)
      } else {
        log.debug("cached character info for character {}", id)
        val resp = CharacterInfoResponse(request, Success(ci))
        request.replyTo.foreach { reply => reply ! resp }
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

}

object EveCharacterInfoApi {

  def props: Props = Props[EveCharacterInfoApi]()

}

/**
 * Eve XML Character Info
 */
class EveCharacterInfoApi extends Actor with ActorLogging with EveXmlApi[CharacterInfo] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def receive: Receive = {

    case request@CharacterInfoRequest(id, replyTo) =>
      val fci: Future[CharacterInfo] = complete(Uri.Query(Pair("characterID", id.toString)))
      fci.onComplete { tci =>
        val resp = CharacterInfoResponse(request, tci)
        request.replyTo.foreach { reply => reply ! resp }
      }

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

    val firstEmployment = parseDatetime(eh.last.startDate).getTime
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



