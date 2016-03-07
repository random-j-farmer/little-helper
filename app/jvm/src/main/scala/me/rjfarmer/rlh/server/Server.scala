package me.rjfarmer.rlh.server

import akka.routing.FromConfig
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve._
import spray.http.HttpHeaders.Authorization
import spray.http._
import spray.httpx.encoding.{Deflate, Gzip, NoEncoding}
import spray.routing.{RequestContext, SimpleRoutingApp}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {

  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

/** holds additional information from the request headers (IGB!) */
final case class RequestHeaderData(clientIP: String,
                                   solarSystem: Option[String],
                                   pilot: Option[String],
                                   jwt: Option[JsonWebToken],
                                   jwtRefreshed: Boolean) {

  def refreshedJsonWebToken: Option[String] = {
    if (jwtRefreshed) {
      // XXX spray bug, double wrap!
      Some(JsonWebToken.encodeBase64(JsonWebToken.sign(jwt.get.payload, Boot.privateConfig.get.jwtSecret)))
    } else None
  }

}


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
                HttpEntity(MediaTypes.`text/html`, Page.skeleton.render)
              }
            } ~
            path("authenticated") {
              // we use a different url because the jwt is stored as cookie in a non-/ path
              complete {
                HttpEntity(MediaTypes.`text/html`, Page.skeleton.render)
              }
            } ~
            path("crestLoginCallback") {
              // redirect with cookie to authenticated, otherwise refreshing the browser will
              // reuse the code which leads to an error
              parameter('code) { code =>
                complete(handleCrestLoginCallback(code))
              }
            } ~
            getFromResourceDirectory("WEB-ROOT")
          } ~
          post {
            path("ajax" / Segments) { s =>
              extract(extractRequestData) { future =>
                complete(future.map { pair =>
                  val (rhd, body) = pair
                  val apiHandler = new ApiRequesthandler(listCharactersCache, dscanResultsCache, rhd)
                  Router.route[Api](apiHandler)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](body)))
                })
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

    privateConfig match {

      case None =>
        Future(
          HttpResponse(StatusCodes.TemporaryRedirect,
            entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, "Get thee gone, fiend!"),
            headers = List(HttpHeaders.Location(Uri("/")))))

      case Some(pc) =>
        Boot.bootSystem.log.info("crestLoginCallback: {}", code)
        CrestLogin.login(pc.crestConfig.clientID, pc.crestConfig.clientSecret, "characterLocationRead", code)
          .flatMap { token =>
            CrestLogin.verify(token)
          }.map { payload =>
            // XXX spray bug parsing bearer token with dots, so we base64 encode again
            val jwt = JsonWebToken.encodeBase64(JsonWebToken.sign(payload, pc.jwtSecret))
            HttpResponse(StatusCodes.TemporaryRedirect,
              headers = List(HttpHeaders.Location(Uri("/authenticated")),
                HttpHeaders.`Set-Cookie`(HttpCookie("jwt", jwt))),
              entity = HttpEntity(MediaTypes.`text/plain`, "Speak, friend, and enter!"))
          }
    }
  }

  private def extractRequestData(ctx: RequestContext): Future[(RequestHeaderData, String)] = {

    import spray.util._

    def optionalValue(lowerCaseName: String): HttpHeader => Option[String] = {
      case HttpHeader(`lowerCaseName`, value) => Some(value)
      case _ => None
    }
    def f(headerName: String) = optionalValue(headerName.toLowerCase)

    val headers = ctx.request.headers

    val forwarded = headers.mapFind(f("X-Forwarded-For"))
    val remoteAddress = headers.mapFind(f("Remote-Address"))
    val defaultSolar = headers.mapFind(f("EVE_SOLARSYSTEMNAME"))
    val ip = forwarded.getOrElse(remoteAddress.get)

    val jsonWebToken = extractJsonWebToken(ctx)
    val pilot: Option[String] = jsonWebToken.fold(headers.mapFind(f("EVE_CHARNAME"))) (x => Some(x.payload.characterName))
    val accessTokenExpired = jsonWebToken.fold(false)(x => x.payload.crestToken.expireTs < System.currentTimeMillis())
    val jsonWebFuture: Future[Option[JsonWebToken]] = refreshJsonWebToken(jsonWebToken, accessTokenExpired)
    val solar = solarSystemFuture(defaultSolar, jsonWebFuture)
    val promise = Promise[(RequestHeaderData, String)]()
    val reqBody = ctx.request.entity.asString

    solar.onComplete {
      case Failure(ex) =>
        bootSystem.log.warning("Solar system future: error: " + ex)
        promise.complete(Success(Pair(RequestHeaderData(ip, defaultSolar, pilot, jsonWebToken, jwtRefreshed = false), reqBody)))
      case Success(ssi) =>
        promise.complete(Success(Pair(RequestHeaderData(ip, ssi, pilot,
          jsonWebFuture.value.get.get, jwtRefreshed = accessTokenExpired), reqBody)))
    }

    promise.future
  }

  private def extractJsonWebToken(ctx: RequestContext): Option[JsonWebToken] = {
    ctx.request.header[Authorization]
      .map { x =>
        val HttpHeaders.Authorization(OAuth2BearerToken(jwt)) = x
        jwt
      }
      // XXX spray bug parsing JWT tokens, so ours are base64 encoded once more
      .map(JsonWebToken.decodeBase64)
      .flatMap(JsonWebToken.verify(_, privateConfig.get.jwtSecret))
  }

  private def refreshJsonWebToken(jsonWebToken: Option[JsonWebToken], accessTokenExpired: Boolean): Future[Option[JsonWebToken]] = {
    if (accessTokenExpired) {
      val jwt = jsonWebToken.get
      val cc = privateConfig.get.crestConfig
      CrestLogin.refresh(cc.clientID, cc.clientSecret, jwt.payload.crestToken)
        // this does not have a valid signature!
        .map(ct => Some(jwt.copy(payload = jwt.payload.copy(crestToken = ct))))
    } else {
      Future(jsonWebToken)
    }
  }

  private def solarSystemFuture(igbSolarSystem: Option[String], jsonWebFuture: Future[Option[JsonWebToken]]): Future[Option[String]] = {
    igbSolarSystem match {
      case Some(solarSystem) =>
        Future(igbSolarSystem)

      case None =>
        jsonWebFuture.flatMap {
          case None =>
            Future(None)

          case Some(jwt) =>
            CrestApi.characterLocation(jwt)

        }
    }
  }

}
