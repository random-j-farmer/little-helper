package me.rjfarmer.rlh.eve

import akka.actor.ActorRef
import akka.event.{Logging, LoggingAdapter, NoLogging}
import akka.io.IO
import akka.pattern.ask
import me.rjfarmer.rlh.retriever.{ResponseBodyDecoder, Retriever}
import me.rjfarmer.rlh.server.Boot
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

import scala.concurrent.{Await, Future, Promise}
import scala.util.Try

abstract class CrestApiBase[T](parseBodyFn: (Uri, String) => T) extends ResponseBodyDecoder {

  // for the implicit actor system
  import Boot._

  def uriHostname: String

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, Boot.ajaxFutureTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  def hostConnectorSetup =   Http.HostConnectorSetup(uriHostname, port=443, sslEncryption = true,
    defaultHeaders = Retriever.defaultHeaders)

  def log: LoggingAdapter = NoLogging

  def complete(request: HttpRequest): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val started = System.currentTimeMillis()
    // use the rest timeout here - this is where we want to fail so the incomplete answer logic works
    val httpFuture = ask(hostConnector, request)(Boot.restTimeout.duration)
    val promise = Promise[T]()
    httpFuture onSuccess {
      case resp: HttpResponse =>
        if (resp.status.isSuccess) {
          log.debug("http get: {} ===> {} in {}ms", request.uri, resp.status.intValue, System.currentTimeMillis - started)
          promise.complete(Try(parseResponseBody(request.uri, decodeResponseBody(resp))))
        } else {
          log.debug("http get error: {} {} after {}ms", resp.status.intValue, resp.entity.data.asString,
            System.currentTimeMillis - started)
          promise.failure(new IllegalArgumentException("http result not ok: " + resp.status.intValue))
        }
    }
    promise.future
  }

  def parseResponseBody(uri: Uri, json: String): T = {
    log.debug("CrestApiBase: parsing response body: {} {}", uri, json)
    parseBodyFn(uri, json)
  }

  def authorizationBearer(token: CrestToken): HttpHeader = {
    HttpHeaders.Authorization(OAuth2BearerToken(token.access_token))
  }

}


object CrestApi {

  def characterLocation(jwt: JsonWebToken) = {
    val crestApi = new CrestApi((uri, json) => json)
    crestApi.complete(crestApi.characterLocationRequest(jwt.payload.characterID, jwt.payload.crestToken))
  }

}

class CrestApi[T] (parseBodyFn: (Uri, String) => T) extends CrestApiBase[T](parseBodyFn) {

  override def uriHostname: String = "crest-tq.eveonline.com"

  def characterLocationRequest(characterID: Long, token: CrestToken): HttpRequest = {
    HttpRequest(GET, Uri(s"/characters/$characterID/location/"), headers = List(authorizationBearer(token)))
  }

}


object CrestLogin {

  def login(clientID: String, clientSecret: String, scope: String, code: String): Future[CrestToken] = {
    val crestLogin = new CrestLogin((uri, json) => upickle.default.read[CrestToken](json))
    crestLogin.complete(crestLogin.loginRequest(clientID, clientSecret, scope, code))
  }

  def verify(token: CrestToken): Future[JsonWebToken.Payload] = {
    val crestLogin = new CrestLogin(parseVerifyBody(token))
    crestLogin.complete(crestLogin.verifyRequest(token))
  }

  private def parseVerifyBody(token: CrestToken) (uri: Uri, json: String): JsonWebToken.Payload = {
    val tree = jawn.ast.JParser.parseFromString(json).get
    JsonWebToken.Payload(tree.get("CharacterName").asString,
      tree.get("CharacterID").asLong,
      token)
  }

}



class CrestLogin[T] (parseBodyFn: (Uri, String) => T) extends CrestApiBase[T](parseBodyFn) {

  override def uriHostname: String = "login.eveonline.com"

  override val log: LoggingAdapter = Logging(Boot.bootSystem, "CrestApi")

  def loginRequest(clientID: String, clientSecret: String, scope: String, code: String): HttpRequest = {
    val body = s"grant_type=authorization_code&scope=$scope&code=$code"
    log.debug("requestEntity: {}", body)
    val entity = HttpEntity(ContentType(MediaTypes.`application/x-www-form-urlencoded`, HttpCharsets.`UTF-8`), body)
    HttpRequest(POST, Uri("/oauth/token"), entity = entity, headers = List(loginAuthorizationHeader(clientID, clientSecret)))
  }

  def loginAuthorizationHeader(clientID: String, clientSecret: String): HttpHeader = {
    log.debug("authorizationHeader: clientID {}, clientSecret {}", clientID, clientSecret)
    val header = HttpHeaders.Authorization(new BasicHttpCredentials(clientID, clientSecret))
    log.debug("final header: {}", header)
    header
  }

  def verifyRequest(token: CrestToken): HttpRequest = {
    HttpRequest(GET, Uri("/oauth/verify"), headers=List(authorizationBearer(token)))
  }

}

/**
 * CrestToken as returned by the Crest API
 *
 * the underscores are used because the input is like that
 *
 * @param access_token  access token
 * @param token_type Bearer
 * @param expires_in validity in seconds
 * @param refresh_token refresh token, can be used to get a new token
 */
final case class CrestToken(access_token: String,
                            token_type: String,
                            expires_in: Int,
                            refresh_token: String)
