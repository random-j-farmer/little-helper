package me.rjfarmer.rlh.eve

import java.util.concurrent.TimeUnit

import akka.actor._
import akka.routing.FromConfig
import akka.pattern.ask
import me.rjfarmer.rlh.api.{EmploymentHistory, CharacterInfo}
import me.rjfarmer.rlh.eve.CharacterInfoApi.{CharacterInfoResponse, CharacterInfoRequest}
import me.rjfarmer.rlh.server.Boot
import spray.caching.{Cache, LruCache}
import spray.http.Uri

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.xml.XML

object CharacterInfoApi {

  private val cache = LruCache[CharacterInfo](maxCapacity = Boot.bootConfig.getInt("little-helper.xml-api.cache.character-info"),
    timeToLive = Duration.create(Boot.bootConfig.getDuration("little-helper.xml-api.cache-ttl.character-info",
    TimeUnit.MILLISECONDS), TimeUnit.MILLISECONDS))

  def props(eveCharacterInfo: ActorRef): Props = Props(new CharacterInfoApi(cache, eveCharacterInfo))

  final case class CharacterInfoRequest(characterID: Long, replyTo: Seq[ActorRef])

  final case class CharacterInfoResponse(request: CharacterInfoRequest, result: Try[CharacterInfo])


  def main(args: Array[String]) = {
    import Boot._

    try {
      val eveCharacterInfo = bootSystem.actorOf(FromConfig.props(EveCharacterInfoApi.props), "eveCharacterInfoPool")
      val characterInfo = bootSystem.actorOf(FromConfig.props(CharacterInfoApi.props(eveCharacterInfo)), "characterInfoPool")

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
class CharacterInfoApi (cache: Cache[CharacterInfo], eveCharacterInfo: ActorRef)
  extends Actor with ActorLogging {

  import scala.concurrent.ExecutionContext.Implicits.global


  override def receive: Receive = {

    case CharacterInfoRequest(id, Seq()) =>
      // for convenience of testing via ask
      self ! CharacterInfoRequest(id, Seq(sender()))

    case request@CharacterInfoRequest(id, replyTo) =>
      cache.get(id) match {
        case Some(fci) =>
          // cached future is already completed, but whatever
          fci onComplete { tci =>
            log.debug("cached character info for character {}", id)
            val resp = CharacterInfoResponse(request, tci)
            request.replyTo.foreach { reply => reply ! resp }
          }
        case None =>
          eveCharacterInfo ! request.copy(replyTo = replyTo :+ self)
      }

    case CharacterInfoResponse(req, tci) =>
      tci match {
        case Success(ci) =>
          log.debug("caching character info for {}", req.characterID)
          cache(req.characterID)(ci)
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
      etxt(elem, "securityStatus").toDouble, age, eh)
  }

}



