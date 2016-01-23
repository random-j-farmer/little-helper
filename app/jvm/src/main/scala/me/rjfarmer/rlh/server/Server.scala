package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.routing.FromConfig
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.CharacterIDApi._
import me.rjfarmer.rlh.eve.CharacterInfoApi.{GroupedCharacterInfoRequest, GroupedCharacterInfoResponse}
import me.rjfarmer.rlh.eve.ZkStatsApi.{GroupedZkStatsRequest, GroupedZkStatsResponse}
import me.rjfarmer.rlh.eve._
import org.ehcache.CacheManagerBuilder
import org.ehcache.config.xml.XmlConfiguration
import spray.can.Http
import spray.http.{HttpEntity, MediaTypes}
import spray.httpx.encoding.{Gzip, NoEncoding, Deflate}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {

  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

class ServerWithIGBData(characterName: Option[String], solarSystemName: Option[String]) extends Api {

  override def listCharacters(request: ListCharactersRequest): Future[ListCharactersResponse] = {
    Server.listCharacters(request.copy(pilot = characterName, solarSystem = solarSystemName))
  }

}

object Server extends SimpleRoutingApp with Api with RequestTimeout with ShutdownIfNotBound {

  import Boot.{bootTimeout => _, _}
  // use longer timeouts here
  implicit val bootTimeout = Boot.requestTimeout(bootConfig, "little-helper.xml-api.ajax-timeout-long")

  val eveCharacterID = bootSystem.actorOf(FromConfig.props(EveCharacterIDApi.props), "eveCharacterIDPool")
  val characterID = bootSystem.actorOf(FromConfig.props(CharacterIDApi.props(cacheManager, eveCharacterID)), "characterIDPool")
  val eveCharacterInfo = bootSystem.actorOf(FromConfig.props(EveCharacterInfoApi.props), "eveCharacterInfoPool")
  val characterInfo = bootSystem.actorOf(FromConfig.props(CharacterInfoApi.props(cacheManager, eveCharacterInfo)), "characterInfoPool")
  val eveZkStats = bootSystem.actorOf(FromConfig.props(RestZkStatsApi.props), "restZkStatsPool")
  val zkStats = bootSystem.actorOf(FromConfig.props(ZkStatsApi.props(cacheManager, eveZkStats)), "zkStatsPool")

  def main(args: Array[String]): Unit = {

    Runtime.getRuntime.addShutdownHook(CacheManagerShutdownHook)

    // needs a multi-second timeout or it will not bind fast enough on openshift
    val response = startServer(bootHost, port = bootPort) {
      (decodeRequest(Gzip) | decodeRequest(Deflate) | decodeRequest(NoEncoding)) {
        compressResponse() {
          get {
            pathSingleSlash {
              complete {
                HttpEntity(MediaTypes.`text/html`, Page.skeleton.render)
              }
            } ~
            getFromResourceDirectory("") ~
            getFromResourceDirectory("META-INF/resources")
          } ~
            post {
              path("ajax" / Segments) { s =>
                optionalHeaderValueByName("EVE_CHARNAME") { charname =>
                  optionalHeaderValueByName("EVE_SOLARSYSTEMNAME") { solarsystem =>
                    extract(ctx => ctx.request.entity.asString ) { e =>
                      val server = new ServerWithIGBData(charname, solarsystem)
                      complete(Router.route[Api](server)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](e))))
                    }
                  }
                }
              }
            }
        }
      }
    }

    // does not work because we hang in opening ehc disk cache when started
    // multiple times
    shutdownIfNotBound(response)
  }


  private def listIds(names: Vector[String]): Future[CharacterIDResponse] = {
    ask(characterID, CharacterIDRequest(names, Map(), None, None))
      .asInstanceOf[Future[CharacterIDResponse]]
  }

  private def characterInfos(ids: Vector[Long]): Future[Map[Long,CharacterInfo]] = {
    ask(characterInfo, GroupedCharacterInfoRequest(ids, None))
      .asInstanceOf[Future[GroupedCharacterInfoResponse]]
      .map(resp => resp.infoById)
  }

  private def zkStats(ids: Vector[Long]): Future[Map[Long, ZkStats]] = {
    ask(zkStats, GroupedZkStatsRequest(ids, None))
      .asInstanceOf[Future[GroupedZkStatsResponse]]
      .map(resp => resp.infoById)
  }

  def listCharacters(req: ListCharactersRequest): Future[ListCharactersResponse] = {
    val ts = System.currentTimeMillis()
    val idsFuture = listIds(req.names)
    val result = Promise[ListCharactersResponse]()

    bootSystem.log.info("listCharacters: {} {}", req.version, BuildInfo.version)
    if (req.version != BuildInfo.version) {
      result.success(ListCharactersResponse(Some("Client version does not match server, please reload the page (F5)."),
        req.solarSystem,
        Vector()))
    } else {
      idsFuture.onComplete {
        case Success(idResp) =>
          val pureIds = idResp.fullResult.values.map(_.characterID).toVector
          bootSystem.log.warning("unknown character names: {}", idResp.unknownNames.mkString(", "))
          val f1 = characterInfos(pureIds)
          val f2 = zkStats(pureIds)
          f1.zip(f2).onComplete {
            case Success(Pair(infoMap, zkMap)) =>
              bootSystem.log.info("listCharacters: successful response for {} names in {}ms",
                req.names.size, System.currentTimeMillis() - ts)
              val cis = idResp.allNames.map { name =>
                val id: Long = idResp.fullResult.get(name).map(ian => ian.characterID).getOrElse(0L)
                // no results for key 0L, so if the id was not resolved, we return None
                CharInfo(name, infoMap.get(id), zkMap.get(id))
              }.sorted(ByDestroyed)
              result.success(ListCharactersResponse(None, req.solarSystem, cis))
            case Failure(ex) =>
              bootSystem.log.error("listCharacters: received error: {}", ex)
              result.failure(ex)
          }
        case Failure(ex) =>
          result.failure(ex)
      }
    }
    result.future
  }

}


object Boot extends RequestTimeout {

  val bootConfig = ConfigFactory.load()
  val bootHost = bootConfig.getString("http.host")
  val bootPort = bootConfig.getInt("http.port")
  val minRefreshStale = bootConfig.getInt("little-helper.xml-api.refresh-stale")
  val cacheManager = CacheManagerBuilder.newCacheManager(new XmlConfiguration(getClass.getClassLoader.getResource("little-cache.xml")))

  cacheManager.init()

  implicit val bootSystem = ActorSystem("little-helper", bootConfig)
  implicit val bootTimeout = requestTimeout(bootConfig, "little-helper.xml-api.ajax-timeout")


  object CacheManagerShutdownHook extends Thread {

    override def run(): Unit = {
      println("Shutting down actor system")
      bootSystem.shutdown()
      println("Shutting down cache manager")
      cacheManager.close()
    }

  }
}

object ByDestroyed extends Ordering[CharInfo] {
  override def compare(x: CharInfo, y: CharInfo): Int = {
    val yi = y.recentKills.getOrElse(0)
    val xi = x.recentKills.getOrElse(0)
    yi.compareTo(xi)
  }
}

trait RequestTimeout {
  import scala.concurrent.duration._

  def requestTimeout(config: Config, configKey: String): Timeout = {
    val t = config.getString(configKey)
    val d = Duration(t)
    FiniteDuration(d.length, d.unit)
  }
}

trait ShutdownIfNotBound {
  import scala.concurrent.{ExecutionContext, Future}

  def shutdownIfNotBound(f: Future[Any])
                        (implicit system: ActorSystem, ec: ExecutionContext) = {
    f.mapTo[Http.Event].map {
      case Http.Bound(address) =>
        println(s"Interface bound to $address")

      case Http.CommandFailed(cmd) =>
        println(s"nterface could not bind: ${cmd.failureMessage}, shutting down.")
        system.shutdown()
    }.recover {
      case e: Throwable =>
        println(s"Unexpexted error binding to HTTP: ${e.getMessage}, shutting down.")
        system.shutdown()
    }
  }
}



