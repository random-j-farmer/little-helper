package me.rjfarmer.rlh.eve

import akka.actor._
import akka.event.Logging
import akka.io.IO
import akka.pattern.ask
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import me.rjfarmer.rlh.api.{CharacterInfo, CharacterIDAndName}
import me.rjfarmer.rlh.server.Boot
import me.rjfarmer.rlh.server.Boot._
import spray.caching.LruCache
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

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
            characterInfoImpl.complete(args(0))
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

  def listCharacters(names: Seq[String]): Future[Seq[CharacterIDAndName]] = characterIDImpl.complete(names)

  def characterInfo(characterID: String): Future[CharacterInfo] = characterInfoImpl.complete(characterID)
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

    val goodNames = names.map(_.trim).filterNot(_.isEmpty)
    val namesAndCached = goodNames
      .map((str) => Pair(str, cache.get(str)))

    val defined = Map[String,CharacterIDAndName]() ++ namesAndCached.filterNot(_._2 == null)
    val undefinedNames = namesAndCached.filter(_._2 == null).map(_._1)
    log.debug("complete: {} cached/ {} not in cache", defined.size, undefinedNames.size)

    val result = Promise[Type]()

    if (undefinedNames.isEmpty) {
      result.success(goodNames map(defined(_)))
    } else {
      // XXX if more than a hundred or so ==> multiple requests
      val pairs = undefinedNames map { Pair("names", _) }
      val apiResult: Future[Type] = complete(Uri.Query(pairs:_*))

      apiResult.onFailure { case ex => result.failure(ex) }
      apiResult.onSuccess { case idsAndNames: Type =>
        idsAndNames.foreach { ian =>
          // log.debug("new cache entry: {}", ian)
          cache.put(ian.characterName, ian)
        }
        val combined = defined ++ idsAndNames.map((x) => Pair(x.characterName, x))
        // combined may be smaller than goodnames - only existing characters are returned
        result.success(goodNames.filter(combined.contains(_)).map(combined))
      }
    }

    result.future
  }


  val uriPath = "/eve/CharacterID.xml.aspx"

  def successMessage(xml: String): Seq[CharacterIDAndName] = {
    val elem = XML.loadString(xml)
    // log.debug("xml: {}", xml)
    for {
      row <- elem \\ "row"
      id = row \@ "characterID"
      if id != "0"
    } yield {
      CharacterIDAndName(id, row \@ "name")
    }
  }
}

/**
 * Translate a CharacterID(Long) into XML
 */
class CharacterInfoImplEve extends EveXmlApiImpl[CharacterInfo] {

  private val cache = LruCache[Type](maxCapacity = Boot.bootConfig.getInt("little-helper.xml-api.cache.character-ids"))

  def complete(characterID: String): Future[Type] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    cache(characterID)(complete(Uri.Query(Pair("characterID", characterID))))
  }

  val uriPath = "/eve/CharacterInfo.xml.aspx"

  def successMessage(xml: String): CharacterInfo = {
    // log.debug("xml: {}", xml)
    val elem = (XML.loadString(xml) \\ "result")(0)
    CharacterInfo(etxt(elem, "characterID"),
      etxt(elem, "characterName"),
      etxt(elem, "race"), etxt(elem, "bloodline"), etxt(elem, "ancestry"),
      etxt(elem, "corporationID"), etxt(elem, "corporation"), etxt(elem, "corporationDate"),
      opttxt(elem, "allianceID"), opttxt(elem, "alliance"), opttxt(elem, "allianceDate"),
      etxt(elem, "securityStatus").toDouble)
  }
}

