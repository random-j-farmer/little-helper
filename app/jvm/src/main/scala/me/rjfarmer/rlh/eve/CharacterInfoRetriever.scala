package me.rjfarmer.rlh.eve

import java.text.SimpleDateFormat
import java.util.{TimeZone, Date}

import akka.actor.{ActorRef, Props}
import akka.pattern.ask
import akka.util.Timeout
import me.rjfarmer.rlh.api.{CharacterInfo, EmploymentHistory, WebserviceRequest}
import me.rjfarmer.rlh.retriever._
import org.ehcache.CacheManager
import spray.can.Http
import spray.http.Uri

import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.xml.{Node, XML}

object CharacterInfoRetriever {

  val hostConnectorSetup = Http.HostConnectorSetup("api.eveonline.com", port=443, sslEncryption = true,
    defaultHeaders = Retriever.defaultHeaders)

  private val retrieveQueue = RetrieveQueue[Long]()

  def props(cacheManager: CacheManager, retrieveTimeout: FiniteDuration): Props = {
    val ehCache = cacheManager.getCache("characterInfoCache", classOf[java.lang.Long], classOf[CharacterInfo])
    Retriever.props[Long, CharacterInfo](new EhcRetrieveLongCache[CharacterInfo](ehCache), retrieveQueue,
      CharacterInfoBodyParser, retrieveTimeout, hostConnectorSetup)
  }

  def characterInfo(characterInfoRetriever: ActorRef, wsr: WebserviceRequest, ids: Vector[Long], askTimeout: Timeout): Future[Map[Long, CharacterInfo]] = {

    ask(characterInfoRetriever, CharacterInfoRetriGroup(wsr, ids, None))(askTimeout)
      .asInstanceOf[Future[Map[Long, CharacterInfo]]]

  }

}

final case class CharacterInfoRetrievable(key: Long, priority: Int, replyTo: ActorRef) extends Retrievable[Long] {

  private[this] val uriPath: String = "/eve/CharacterInfo.xml.aspx"

  override def httpGetUri: Uri = Uri(path = Uri.Path(uriPath), query = Uri.Query("characterID" -> key.toString))

}

final case class CharacterInfoRetriGroup (wsr: WebserviceRequest, items: Vector[Long], replyTo: Option[ActorRef]) extends RetriGroup[Long] {

  override def retrievable(k: Long, priority: Int, replyTo: ActorRef): Retrievable[Long] = {
    CharacterInfoRetrievable(k, priority, replyTo)
  }

}

trait EveXmlParser {

  def etxt(elem: Node, child: String): String = (elem \ child).text

  def opttxt(elem: Node, child: String): Option[String] = {
    val el = elem \ child
    if (el.isEmpty) None else Some(el.text)
  }

  /**
   * Parses the date/time string as UTC date time.
   *
   * Example input: 2008-04-15 08:17:23
   * @param dt string input
   * @return
   */
  def parseDatetime(dt: String): Date = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    sdf.parse(dt)
  }

}

object CharacterInfoBodyParser extends BodyParser[Long, CharacterInfo] with EveXmlParser {

  override def parseBody(key: Long, xml: String): CharacterInfo = {
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
