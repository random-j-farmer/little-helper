package me.rjfarmer.rlh.server

import java.util.Base64

import akka.routing.FromConfig
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve._
import spray.http._
import spray.httpx.encoding.{Deflate, Gzip, NoEncoding}
import spray.routing.{RequestContext, SimpleRoutingApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {

  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

/** holds additional information from the request headers (IGB!) */
final case class RequestHeaderData(clientIP: String, solarSystem: Option[String], pilot: Option[String])


/** handles the client server api on the server side */
class ApiRequesthandler(listCharactersCache: ListCharactersResponseCache,
                        dscanResponseCache: DScanResponseCache,
                        rhd: RequestHeaderData) extends Api {

  override def listCharacters(request: ListCharactersRequest): Future[ListCharactersResponse] = {
    ListCharactersRequestHandler(listCharactersCache).handleRequest(rhd, request)
  }

  override def cachedCharacters(request: CachedCharactersRequest): Future[Option[ListCharactersResponse]] = {
    ListCharactersRequestHandler(listCharactersCache).cachedResponse(rhd, request)
  }

  override def parseDScan(req: DScanParseRequest): Future[DScanParseResponse] = {
    DScanRequestHandler(dscanResponseCache).handleRequest(rhd, req)
  }

  override def cachedDScan(request: CachedDScanRequest): Future[Option[DScanParseResponse]] = {
    DScanRequestHandler(dscanResponseCache).cachedResponse(rhd, request)
  }
}

object Server extends SimpleRoutingApp with RequestTimeout with ShutdownIfNotBound {

  import Boot._

  val clientVersionError = "Client version does not match server, please reload the page (F5)."

  val characterID = bootSystem.actorOf(FromConfig.props(CharacterIDApi.props(cacheManager)), "characterIDPool")
  val characterInfoRetriever = bootSystem.actorOf(FromConfig.props(CharacterInfoRetriever.props(cacheManager,
    restTimeout.duration)), "characterInfoRetrievers")
  val zkStatsRetriever = bootSystem.actorOf(FromConfig.props(ZkStatsRetriever.props(cacheManager,
    restTimeout.duration)), "zkStatsRetrievers")

  val dscanResultsCache = new DScanResponseCache(cacheManager.getCache("dscanResultsCache", classOf[String], classOf[DScanParseResponse]))
  val listCharactersCache = new ListCharactersResponseCache(cacheManager.getCache("listCharactersCache", classOf[String], classOf[ListCharactersResponse]))


  def main(args: Array[String]): Unit = {

    Runtime.getRuntime.addShutdownHook(CacheManagerShutdownHook)

    // needs a multi-second timeout or it will not bind fast enough on openshift
    val response = startServer(bootHost, port = bootPort) {
      (decodeRequest(Gzip) | decodeRequest(Deflate) | decodeRequest(NoEncoding)) {
        compressResponse() {
          get {
            pathSingleSlash {
              complete {
                HttpEntity(MediaTypes.`text/html`, Page.skeleton(None).render)
              }
            } ~
            path("crestLoginCallback") {
              parameter('code) { code =>
                complete(handleCrestLoginCallback(code))
              }
            } ~
            getFromResourceDirectory("") ~
            getFromResourceDirectory("META-INF/resources")
          } ~
          post {
            path("ajax" / Segments) { s =>
              extract(extractRequestData) { pair =>
                val (rhd, body) = pair
                val apiHandler = new ApiRequesthandler(listCharactersCache, dscanResultsCache, rhd)
                complete(Router.route[Api](apiHandler)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](body))))
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

  def handleCrestLoginCallback(code: String): Future[HttpResponse] = {

    crestConfig match {

      case Failure(ex) =>
        Boot.bootSystem.log.warning("crestLoginCallback but no crest configured!: {}", code)
        Future(
          HttpResponse(StatusCodes.TemporaryRedirect,
            entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Get thee gone, fiend!"),
            headers = List(HttpHeaders.Location(Uri("/")))))

      case Success(cc) =>
        Boot.bootSystem.log.info("crestLoginCallback: {}", code)
        CrestLogin.login(cc.clientID, cc.clientSecret, code)
          .map { token =>
            // XXX username/secret
            val jwt = JsonWebToken.sign(JsonWebToken.Payload("???", token), "SIKRIT!")
            val str = upickle.default.write(token)
            val enc = Base64.getEncoder.encodeToString(str.getBytes("UTF-8"))
            HttpResponse(StatusCodes.OK,
              entity = HttpEntity(MediaTypes.`text/html`,
                Page.skeleton(Some(jwt)).render),
              headers = List(HttpHeaders.Location(Uri("/")),
                HttpHeaders.`Set-Cookie`(HttpCookie("crestToken", enc,
                  path = Some("/NoSuchPathUsed"),
                  maxAge = Some(token.expires_in)))))
          }
    }
  }

  private def extractRequestData(ctx: RequestContext): (RequestHeaderData, String) = {

    import spray.util._

    def optionalValue(lowerCaseName: String): HttpHeader => Option[String] = {
      case HttpHeader(`lowerCaseName`, value) => Some(value)
      case _ => None
    }
    def f(headerName: String) = optionalValue(headerName.toLowerCase)

    val headers = ctx.request.headers

    val forwarded = headers.mapFind(f("X-Forwarded-For"))
    val remoteAddress = headers.mapFind(f("Remote-Address"))
    Pair(RequestHeaderData(forwarded.getOrElse(remoteAddress.get),
      headers.mapFind(f("EVE_SOLARSYSTEMNAME")), headers.mapFind(f("EVE_CHARNAME"))),
      ctx.request.entity.asString)

  }

}
