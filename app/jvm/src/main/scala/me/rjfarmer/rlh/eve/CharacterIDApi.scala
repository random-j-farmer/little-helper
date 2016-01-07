package me.rjfarmer.rlh.eve

import akka.actor._
import akka.routing.FromConfig
import akka.pattern.ask
import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import me.rjfarmer.rlh.api.CharacterIDAndName
import me.rjfarmer.rlh.eve.CharacterIDApi.{CharacterIDResponse, CharacterIDRequest}
import me.rjfarmer.rlh.server.Boot
import spray.http.Uri

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.XML

/**
 * Helper for batching of character id lookups.
 *
 * Character ID lookup is done in batches of up to 100 names,
 * but the caching is done individually.
 *
 */
trait CharacterIDBatcher {


  /**
   * Partition the names into unknown names and a map with cached results.
   *
   * The returned names are normalized (trimmed and lowercase) as
   * are the map keys.
   *
   * @param names a sequence of names to look up
   * @return a triple of undefined names (normalized), all names (normalized), map of string to result
   */
  def partitionNames(names: Seq[String])
  : (Seq[String], Seq[String], Map[String, CharacterIDAndName]) = {
    // LOWER CASE!
    val allNames = names
      .map { _.trim }
      .filterNot { _.isEmpty }
      .map { _.toLowerCase }
    val namesAndCached = allNames
      .map((str) => Pair(str, mapName(str)))

    val defined = Map[String,CharacterIDAndName]() ++
      namesAndCached.filter(_._2.isDefined).map((p) => (p._1, p._2.get))
    val undefinedNames = namesAndCached.filter(_._2.isEmpty).map(_._1)

    (undefinedNames, allNames, defined)
  }

  /**
   * Maps a (normalized) name to an optional cached value
   */
  def mapName(name: String): Option[CharacterIDAndName]


  // extract ids and names
  // note that characterID is set to "0" for invalid character names

  /**
   * Extract ids and names.
   *
   * Note that characterID is set to "0" for invalid character names by the Eve XML Api,
   * these are filtered out.
   *
   * @param m map of string to result
   * @param names normalized names
   * @return
   */
  def extractIdAndNames(m: Map[String, CharacterIDAndName], names: Seq[String]) = {
    names.filter(m.contains)
      .map(m)
      .filter { ian => ian.characterID != 0 }
  }

}


object CharacterIDApi {

  private val cache: ConcurrentLinkedHashMap[String, CharacterIDAndName] = new ConcurrentLinkedHashMap.Builder[String, CharacterIDAndName]()
    .initialCapacity(16)
    .maximumWeightedCapacity(Boot.bootConfig.getInt("little-helper.xml-api.cache.character-ids"))
    .build()

  def props(eveCharacterID: ActorRef): Props = Props(new CharacterIDApi(cache, eveCharacterID))

  final case class CharacterIDRequest(names: Seq[String], alreadyKnown: Seq[CharacterIDAndName], replyTo: Seq[ActorRef])

  /**
   * Character ID Response.
   *
   * results values that are already in request.alreadyKnown are not repeated in result.
   *
   * @param request request for this response
   * @param result result sequence or error
   */
  final case class CharacterIDResponse(request: CharacterIDRequest, result: Try[Seq[CharacterIDAndName]]) {

    /** results and already known (cached) values */
    def fullResult: Try[Seq[CharacterIDAndName]] = result map { ians => request.alreadyKnown ++ ians }

  }


  def main(args: Array[String]) = {
    import Boot._

    try {
      val eveCharacterID = bootSystem.actorOf(FromConfig.props(EveCharacterIDApi.props), "eveCharacterIDPool")
      val characterID = bootSystem.actorOf(FromConfig.props(CharacterIDApi.props(eveCharacterID)), "characterIDPool")

      println("ARGS: " + args.mkString(", "))

      val rslt = Await.result(ask(characterID, CharacterIDRequest(args.toSeq, Seq(), Seq())), bootTimeout.duration).asInstanceOf[CharacterIDResponse]
      println("RESULT: " + rslt.fullResult)
    } finally {
      bootSystem.shutdown()
    }
  }

}



/**
 * Main Character ID Lookup.
 *
 * This actor only does in memory-caching, everything else is delegated
 * to either a database cache or eve xml webservice.
 *
 * The in-memory cache is shared between all instances of this actor.
 *
 */
class CharacterIDApi (cache: ConcurrentLinkedHashMap[String, CharacterIDAndName],
                       eveCharacterID: ActorRef)
  extends Actor with ActorLogging with CharacterIDBatcher {


  /**
   * Maps a (normalized) name to an optional cached value
   */
  override def mapName(name: String): Option[CharacterIDAndName] = Option(cache.get(name))

  override def receive: Receive = {

    case CharacterIDRequest(names, alreadyKnown, Seq()) =>
      // for calling via ask, empty replyTo will be filled in with sender()
      self ! CharacterIDRequest(names, alreadyKnown, Seq(sender()))

    case request @ CharacterIDRequest(names, alreadyKnown, replyTo) =>
      log.debug("request for {}", names.mkString)
      val (undefinedNames, allNames, defined) = partitionNames(names)
      log.debug("character id request: {} cached/ {} not in cache", defined.size, undefinedNames.size)

      if (undefinedNames.isEmpty) {
        replyTo foreach { ref =>
          ref ! CharacterIDResponse(request, Try(extractIdAndNames(defined, allNames)))
        }
      } else {
        eveCharacterID ! CharacterIDRequest(undefinedNames, defined.values.toSeq, replyTo :+ self)
      }

    case CharacterIDResponse(request, result) =>
      result match {
        case Success(ians) =>
          log.debug("caching characters: {}", ians.map(_.characterName).mkString(", "))
          ians foreach { ian => cache.put(ian.characterName, ian) }
        case Failure(ex) =>
          log.debug("can not cache failed request: {} {}", request, ex)
      }

    case msg =>
      log.warning("unknown message: {}", msg)
  }

}


object EveCharacterIDApi {

  def props: Props = Props[EveCharacterIDApi]()

}

/**
 * Eve XML Api /eve/CharacterID.xml.aspx
 *
 */
class EveCharacterIDApi extends Actor with ActorLogging with EveXmlApi[Seq[CharacterIDAndName]] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def receive: Actor.Receive = {

    case request @ CharacterIDRequest(names, alreadyKnown, replyTo) =>
      log.debug("request {}", request)
      completeGrouped(names)
        .onComplete { _try =>
          val resp = CharacterIDResponse(request, _try)
          replyTo foreach { actor => actor ! resp }
        }

    case msg =>
      log.warning("unknown message: {}", msg)
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
  def completeGrouped(names: Seq[String]): Future[Seq[CharacterIDAndName]] = {
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
