package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.routing.FromConfig
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve._
import me.rjfarmer.rlh.retriever.PriorityConfig
import me.rjfarmer.rlh.shared.{ClientConfig, SharedConfig}
import org.ehcache.CacheManagerBuilder
import org.ehcache.config.xml.XmlConfiguration
import spray.can.Http
import spray.http.{HttpEntity, HttpHeader, MediaTypes}
import spray.httpx.encoding.{Deflate, Gzip, NoEncoding}
import spray.routing.{RequestContext, SimpleRoutingApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {

  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

case class ServerWithRequestData(clientIP: String, requestData: String, solarSystem: Option[String], pilot: Option[String])
  extends Api with WebserviceRequest {

  override def listCharacters(request: ListCharactersRequest): Future[ListCharactersResponse] = {
    Server.listCharacters(request.copy(clientIP = clientIP, pilot = pilot, solarSystem = solarSystem))
  }

  override def cachedCharacters(request: CachedCharactersRequest): Future[Option[ListCharactersResponse]] = {
    Server.cachedCharacters(request.copy(clientIP = clientIP, pilot = pilot, solarSystem = solarSystem))
  }

  override def parseDScan(request: DScanParseRequest): Future[DScanParseResponse] = {
    Server.parseDScan(request.copy(clientIP = clientIP, pilot = pilot, solarSystem = solarSystem))
  }

  override def cachedDScan(request: CachedDScanRequest): Future[Option[DScanParseResponse]] = {
    Server.cachedDScan(request.copy(clientIP = clientIP, pilot = pilot, solarSystem = solarSystem))
  }

}

object Server extends SimpleRoutingApp with Api with RequestTimeout with ShutdownIfNotBound {

  import Boot._

  val clientVersionError = "Client version does not match server, please reload the page (F5)."

  val characterID = bootSystem.actorOf(FromConfig.props(CharacterIDApi.props(cacheManager)), "characterIDPool")
  val characterInfoRetriever = bootSystem.actorOf(FromConfig.props(CharacterInfoRetriever.props(cacheManager,
    restTimeout.duration)), "characterInfoRetrievers")
  val zkStatsRetriever = bootSystem.actorOf(FromConfig.props(ZkStatsRetriever.props(cacheManager,
    restTimeout.duration)), "zkStatsRetrievers")

  val dscanResultsCache = cacheManager.getCache("dscanResultsCache", classOf[java.lang.String], classOf[DScanParseResponse])
  val listCharactersCache = cacheManager.getCache("listCharactersCache", classOf[java.lang.String], classOf[ListCharactersResponse])


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
              extract(extractRequestData) { srd =>
                complete(Router.route[Api](srd)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](srd.requestData))))
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

  private def extractRequestData(ctx: RequestContext): ServerWithRequestData = {

    import spray.util._

    def optionalValue(lowerCaseName: String): HttpHeader => Option[String] = {
      case HttpHeader(`lowerCaseName`, value) ⇒ Some(value)
      case _                                  ⇒ None
    }
    def f(headerName: String) = optionalValue(headerName.toLowerCase)

    val headers = ctx.request.headers

    val forwarded = headers.mapFind(f("X-Forwarded-For"))
    val remoteAddress = headers.mapFind(f("Remote-Address"))
    ServerWithRequestData(forwarded.getOrElse(remoteAddress.get), ctx.request.entity.asString,
      headers.mapFind(f("EVE_SOLARSYSTEMNAME")), headers.mapFind(f("EVE_CHARNAME")))
  }

  override def listCharacters(req: ListCharactersRequest): Future[ListCharactersResponse] = {
    ListCharactersRequestHandler(ListCharactersResponseCache(listCharactersCache)).handleRequest(req)
  }

  override def cachedCharacters(request: CachedCharactersRequest): Future[Option[ListCharactersResponse]] = {
    ListCharactersRequestHandler(ListCharactersResponseCache(listCharactersCache)).cachedResponse(request.cacheKey)
  }

  override def parseDScan(req: DScanParseRequest): Future[DScanParseResponse] = {
    DScanRequestHandler(DScanResponseCache(dscanResultsCache)).handleRequest(req)
  }

  override def cachedDScan(request: CachedDScanRequest): Future[Option[DScanParseResponse]] = {
    DScanRequestHandler(DScanResponseCache(dscanResultsCache)).cachedResponse(request.cacheKey)
  }

}


object BootLoader {

  var testEnvironment: Boolean = false

  def cacheManagerConfiguration = {
    val fn = if (testEnvironment) "little-cache-test.xml" else "little-cache.xml"
    new XmlConfiguration(getClass.getClassLoader.getResource(fn))
  }

}

object Boot extends RequestTimeout {

  val bootConfig = ConfigFactory.load()
  val bootHost = bootConfig.getString("http.host")
  val bootPort = bootConfig.getInt("http.port")

  val cacheManager = CacheManagerBuilder.newCacheManager(BootLoader.cacheManagerConfiguration)

  import collection.JavaConversions._

  val priorityConfig = PriorityConfig(
    bootConfig.getIntList("little-helper.priorities-by-size").toVector.map(_.toInt),
    bootConfig.getIntList("little-helper.promote-stales").toVector.map(_.toInt),
    bootConfig.getInt("little-helper.stale-priority-offset")
  )

  cacheManager.init()

  implicit val bootSystem = ActorSystem("little-helper", bootConfig)
  implicit val ajaxFutureTimeout = requestTimeout(bootConfig, "little-helper.ajax-future-timeout")
  val restTimeout = requestTimeout(bootConfig, "little-helper.rest-timeout")
  val staleIfOlderThan = requestTimeout(bootConfig, "little-helper.stale-if-older-than")
  SharedConfig.client = ClientConfig(BuildInfo.version, staleIfOlderThan.duration.toMillis)

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
    val ya = y.characterAge.getOrElse(-1.0d)
    val xa = x.characterAge.getOrElse(-1.0d)
    val compKilled = yi.compareTo(xi)
    if (compKilled != 0) compKilled else ya.compareTo(xa)
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



