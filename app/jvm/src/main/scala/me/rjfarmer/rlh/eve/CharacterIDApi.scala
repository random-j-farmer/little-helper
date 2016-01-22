package me.rjfarmer.rlh.eve

import akka.actor._
import me.rjfarmer.rlh.api.CharacterIDAndName
import me.rjfarmer.rlh.eve.CharacterIDApi.{CharacterIDRequest, CharacterIDResponse}
import org.ehcache.{Cache, CacheManager}
import spray.http.Uri

import scala.concurrent.Future
import scala.util.{Failure, Success}
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
   * @return a tupel4 of undefined names (normalized), all names (normalized),
   *         map of string to cached result
   */
  def partitionNames(names: Vector[String])
  : (Vector[String], Vector[String], Map[String, CharacterIDAndName]) = {
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
   * @param names map of normalized names to character id and names
   * @return
   */
  def extractIdAndNames(m: Map[String, CharacterIDAndName], names: Vector[String]): Vector[CharacterIDAndName] = {
    names.filter(m.contains)
      .map(m)
      .filter { ian => ian.characterID != 0}
  }

}


object CharacterIDApi {


  def props(cacheManager: CacheManager, eveCharacterID: ActorRef): Props = {
    val cache = cacheManager.getCache("characterIDCache", classOf[String], classOf[CharacterIDAndName])
    Props(new CharacterIDApi(cache, eveCharacterID))
  }

  final case class CharacterIDRequest(names: Vector[String],
                                      cached: Map[String, CharacterIDAndName],
                                      replyTo: Option[ActorRef],
                                      cacheTo: Option[ActorRef])

  /**
   * Character ID Response.
   *
   * results values that are already in request.alreadyKnown are not repeated in result.
   *
   * @param request request for this response
   * @param result result ian sequence
   * @param unresolved any unresolved names if errors occurred. normalized form, i.e. play nice with the names in the result
   */
  final case class CharacterIDResponse(request: CharacterIDRequest, result: Vector[CharacterIDAndName], unresolved: Set[String]) {

    /** results and already known (cached) values, without 0 ids*/
    def fullResult: Map[String, CharacterIDAndName] = {
      (request.cached ++ result.map(ian => ian.characterName -> ian)).filter(x => x._2.characterID != 0)
    }

    /** all names - full results and unresolved (= normalized) names */
    def allNames: Vector[String] = {
      (Set() ++ unresolved ++ fullResult.keys).toVector
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
class CharacterIDApi (cache: Cache[String, CharacterIDAndName],
                       eveCharacterID: ActorRef)
  extends Actor with ActorLogging with CharacterIDBatcher {


  /**
   * Maps a (normalized) name to an optional cached value
   */
  override def mapName(name: String): Option[CharacterIDAndName] = Option(cache.get(name))

  override def receive: Receive = {

    case CharacterIDRequest(names, cached, None, cacheTo) =>
      // for calling via ask, empty replyTo will be filled in with sender()
      self ! CharacterIDRequest(names, cached, Some(sender()), cacheTo)

    case request @ CharacterIDRequest(names, _, Some(replyTo), _) =>
      // this is the cache!  we expect no incoming cache information
      log.debug("request for {} ids", names.length)
      val (undefinedNames, allNames, defined) = partitionNames(names)
      log.info("character id request: {} cached/ {} not in cache",
        defined.size, undefinedNames.size)

      if (undefinedNames.isEmpty) {
        replyTo ! CharacterIDResponse(request, extractIdAndNames(defined, allNames), Set())
      } else {
        eveCharacterID ! CharacterIDRequest(undefinedNames, defined, Some(replyTo), Some(self))
      }

    case CharacterIDResponse(req, ians, _) =>
      log.debug("caching {} characters", ians.size)
      ians.foreach { ian => cache.put(ian.characterName, ian) }

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
class EveCharacterIDApi extends Actor with ActorLogging with EveXmlApi[Vector[CharacterIDAndName]] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def receive: Actor.Receive = {

    case request @ CharacterIDRequest(names, _, _, _) =>
      log.debug("request: looking up {} names", names.size)
      completeGrouped(request)

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
   * @param req CharacterIDRequest
   * @return
   */
  def completeGrouped(req: CharacterIDRequest): Unit = {

    val grouped = req.names.map { name => "names" -> name } grouped 100
    val groupedFutures = grouped.map { pairs =>
      val future = complete(Uri.Query(pairs:_*))
      future.onSuccess { case vec => req.cacheTo.foreach {cc => cc ! CharacterIDResponse(req, vec, Set()) } }
      future
    }
    Future.sequence(groupedFutures)
      .onComplete {
        case Success(groups) =>
          val resp = CharacterIDResponse(req, groups.flatten.toVector, Set())
          req.replyTo.foreach { re => re ! resp }
        case Failure(ex) =>
          log.error("completedGrouped: received error: {}", ex)
          var allWeGot: Vector[CharacterIDAndName] = Vector()
          for (f <- groupedFutures; ians <- f) {
            allWeGot ++= ians
          }
          val namesSet: Set[String] = Set() ++ req.names
          val gotSet: Set[String] = Set() ++ allWeGot.map(_.characterName)
          req.replyTo.foreach { re => re ! CharacterIDResponse(req, allWeGot, namesSet.diff(gotSet)) }
      }
  }

  val uriPath = "/eve/CharacterID.xml.aspx"

  // parse the result XML, name is LOWERCASE for caching
  def successMessage(xml: String): Vector[CharacterIDAndName] = {
    val elem = XML.loadString(xml)
    // log.debug("xml: {}", xml)
    val ts = System.currentTimeMillis()
    (for {
      row <- elem \\ "row"
      id = row \@ "characterID"
    } yield {
      CharacterIDAndName(id.toLong, (row \@ "name").toLowerCase, ts)
    }).toVector
  }

}
