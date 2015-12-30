package me.rjfarmer.rlh.eve

import java.text.SimpleDateFormat
import java.util.concurrent.TimeUnit
import java.util.{TimeZone, Date}

import akka.actor._
import akka.event.Logging
import akka.io.IO
import akka.pattern.ask
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import me.rjfarmer.rlh.api.{EmploymentHistory, CharacterInfo, CharacterIDAndName}
import me.rjfarmer.rlh.server.Boot
import me.rjfarmer.rlh.server.Boot._
import spray.caching.LruCache
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

import scala.concurrent.duration.Duration
import scala.concurrent.{Promise, Future, Await}
import scala.util.Try
import scala.xml.{Node, XML}


object EveXmlApi {

  import Boot._

  private val characterIDImpl = new CharacterIDImplEve

  private val characterInfoImpl = new CharacterInfoImplEve

  def main(args: Array[String]): Unit = {
    try {
      println(
        Await.result(
          if (args(0) matches """[0-9]+""") {
            characterInfoImpl.complete(args(0).toLong)
          } else {
            characterIDImpl.complete(args.toSeq)
          },
          bootTimeout.duration
        )
      )
    } finally {
      bootSystem.shutdown()
    }
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

  def listCharacters(names: Seq[String]): Future[Seq[CharacterIDAndName]] = characterIDImpl.complete(names)

  def characterInfo(characterID: Long): Future[CharacterInfo] = characterInfoImpl.complete(characterID)
}



trait EveXmlApiImpl[T] {

  type Type = T

  def uriPath: String

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, bootTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  def hostConnectorSetup = Http.HostConnectorSetup("api.eveonline.com", port=443, sslEncryption = true)

  def successMessage(xml: String): T

  val log = Logging(Boot.bootSystem, getClass)

  def httpGetUri(query: Uri.Query): Uri = Uri(path = Uri.Path(uriPath), query = query)

  def complete(query: Uri.Query): Future[T] = {
    import Boot._
    import scala.concurrent.ExecutionContext.Implicits.global
    val uri = httpGetUri(query)
    log.debug("http get: {}", uri)
    val httpFuture = ask(hostConnector, HttpRequest(GET, httpGetUri(query)))
    val promise = Promise[T]()
    httpFuture onSuccess {
        case resp: HttpResponse =>
          if (resp.status.isSuccess) {
            log.debug("http get: {} ===> {}", uri, resp.status.intValue)
            promise.complete(Try(successMessage(resp.entity.data.asString)))
          } else {
            log.debug("http get error: {} {}", resp.status.intValue, resp.entity.data.asString)
            promise.failure(new IllegalArgumentException("http result not ok: " + resp.status.intValue))
          }
      }
    promise.future
  }

  def etxt(elem: Node, child: String): String = (elem \ child).text

  def opttxt(elem: Node, child: String): Option[String] = {
    val el = elem \ child
    if (el.isEmpty) None else Some(el.text)
  }

}

/**
 *
 * Translates a CharacterName(String) into XML
 *
 */
class CharacterIDImplEve extends EveXmlApiImpl[Seq[CharacterIDAndName]] {

  private val cache = new ConcurrentLinkedHashMap.Builder[String, CharacterIDAndName]()
    .initialCapacity(16)
    .maximumWeightedCapacity(Boot.bootConfig.getInt("little-helper.xml-api.cache.character-ids"))
    .build()


  /*
   * caching is complicated here because the xml api and our return value
   * works as seqence of items, but we cache individual returns
   */
  def complete(names: Seq[String]): Future[Type] = {

    import scala.concurrent.ExecutionContext.Implicits.global

    // LOWER CASE!
    val goodNames = names
      .map { _.trim }
      .filterNot { _.isEmpty }
      .map { _.toLowerCase }
    val namesAndCached = goodNames
      .map((str) => Pair(str, cache.get(str)))

    val defined = Map[String,CharacterIDAndName]() ++ namesAndCached.filterNot(_._2 == null)
    val undefinedNames = namesAndCached.filter(_._2 == null).map(_._1)
    log.debug("complete: {} cached/ {} not in cache", defined.size, undefinedNames.size)


    if (undefinedNames.isEmpty) {
      // everything is cached already
      Future.successful(extractIdAndNames(defined, goodNames))
    } else {
      // have to retrieve undefined names
      completeUncached(undefinedNames)
        .map { ians =>
          ians.foreach { ian => cache.put(ian.characterName, ian) }
          val combined = defined ++ ians.map { ian => Pair(ian.characterName, ian) }
          extractIdAndNames(combined, goodNames)
        }
    }
  }

  // extract ids and names
  // note that characterID is set to "0" for invalid character names
  private def extractIdAndNames(m: Map[String, CharacterIDAndName], names: Seq[String]) = {
    names.filter(m.contains)
      .map(m)
      .filter { ian => ian.characterID != 0 }
  }

  /**
   * Complete name to id resolving over rest api.
   *
   * When the request gets too big it seems to fail,
   * may be the number of arguments or the URI size.
   *
   * 100-150 seem to be ok, 300 fails.
   *
   * @param names names to look up
   * @return
   */
  def completeUncached(names: Seq[String]): Future[Type] = {
    import scala.concurrent.ExecutionContext.Implicits.global

    val grouped = names map { Pair("names", _) } grouped 100
    Future.sequence(grouped map { pairs => complete(Uri.Query(pairs:_*))})
    .map { groups => groups.flatten.toSeq }
  }

  val uriPath = "/eve/CharacterID.xml.aspx"

  // parse the result XML, name is LOWERCASE for caching
  def successMessage(xml: String): Seq[CharacterIDAndName] = {
    val elem = XML.loadString(xml)
    // log.debug("xml: {}", xml)
    for {
      row <- elem \\ "row"
      id = row \@ "characterID"
    } yield {
      CharacterIDAndName(id.toLong, (row \@ "name").toLowerCase)
    }
  }
}

/**
 * Translate a CharacterID(Long) into XML
 */
class CharacterInfoImplEve extends EveXmlApiImpl[CharacterInfo] {

  private val cache = LruCache[Type](maxCapacity = Boot.bootConfig.getInt("little-helper.xml-api.cache.character-info"),
   timeToLive = Duration.create(Boot.bootConfig.getDuration("little-helper.xml-api.cache-ttl.character-info",
    TimeUnit.MILLISECONDS), TimeUnit.MILLISECONDS))

  def complete(characterID: Long): Future[Type] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    cache.get(characterID) match {
      case Some(fci) => fci
      case None =>
        val future = complete(Uri.Query(Pair("characterID", characterID.toString)))
        // the cache is called as side effect because we only want successful responses in there
        future.foreach { ci => cache(characterID)(ci) }
        future
    }
  }

  val uriPath = "/eve/CharacterInfo.xml.aspx"

  def successMessage(xml: String): CharacterInfo = {
    // log.debug("xml: {}", xml)
    val elem = (XML.loadString(xml) \\ "result")(0)

    val eh = for {
      history <- elem \ "rowset" if history \@ "name" == "employmentHistory"
      row <- history \ "row"
    } yield EmploymentHistory(row \@ "corporationID", row \@ "corporationName", row \@ "startDate")

    val firstEmployment = EveXmlApi.parseDatetime(eh.last.startDate).getTime
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

