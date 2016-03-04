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

object CrestLogin {

  def login(clientID: String, clientSecret: String, code: String): Future[CrestToken] = {
    val loginApi = new CrestLogin()
    loginApi.complete(loginApi.requestEntity(code), headers = List(loginApi.authorizationHeader(clientID, clientSecret)))
  }

}

trait CrestApi[T] extends ResponseBodyDecoder {

  // for the implicit actor system
  import Boot._

  def uriPath: String

  def uriHostname: String

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, Boot.ajaxFutureTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  def hostConnectorSetup =   Http.HostConnectorSetup(uriHostname, port=443, sslEncryption = true,
    defaultHeaders = Retriever.defaultHeaders)

  def httpPostUri: Uri = Uri(path = Uri.Path(uriPath))

  def log: LoggingAdapter = NoLogging

  def complete(requestEntity: HttpEntity, headers: List[HttpHeader] = Nil): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val started = System.currentTimeMillis()
    // use the rest timeout here - this is where we want to fail so the incomplete answer logic works
    val httpFuture = ask(hostConnector, HttpRequest(POST, httpPostUri, entity = requestEntity, headers = headers))(Boot.restTimeout.duration)
    val promise = Promise[T]()
    httpFuture onSuccess {
      case resp: HttpResponse =>
        if (resp.status.isSuccess) {
          log.debug("http get: {} ===> {} in {}ms", httpPostUri, resp.status.intValue, System.currentTimeMillis - started)
          promise.complete(Try(parseResponseBody(httpPostUri, decodeResponseBody(resp))))
        } else {
          log.debug("http get error: {} {} after {}ms", resp.status.intValue, resp.entity.data.asString,
            System.currentTimeMillis - started)
          promise.failure(new IllegalArgumentException("http result not ok: " + resp.status.intValue))
        }
    }
    promise.future
  }

  def parseResponseBody(uri: Uri, json: String): T

}

class CrestLogin extends CrestApi[CrestToken] {

  override def uriHostname: String = "login.eveonline.com"

  override def uriPath: String = "/oauth/token"

  override val log: LoggingAdapter = Logging(Boot.bootSystem, "CrestApi")

  override def parseResponseBody(uri: Uri, json: String): CrestToken = {
    log.debug("CrestLogin: parsing response body: {}", json)
    upickle.default.read[CrestToken](json)
  }

  def authorizationHeader(clientID: String, clientSecret: String): HttpHeader = {
    log.debug("authorizationHeader: clientID {}, clientSecret {}", clientID, clientSecret)
    val header = HttpHeaders.Authorization(new BasicHttpCredentials(clientID, clientSecret))
    log.debug("final header: {}", header)
    header
  }

  def requestEntity(code: String): HttpEntity = {
    val body = s"grant_type=authorization_code&code=$code"
    log.debug("requestEntity: {}", body)
    HttpEntity(ContentType(MediaTypes.`application/x-www-form-urlencoded`, HttpCharsets.`UTF-8`), body)
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
