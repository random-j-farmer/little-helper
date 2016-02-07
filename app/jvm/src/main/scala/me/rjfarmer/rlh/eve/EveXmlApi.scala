package me.rjfarmer.rlh.eve

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import akka.io.IO
import akka.pattern.ask
import me.rjfarmer.rlh.retriever.ResponseBodyDecoder
import me.rjfarmer.rlh.server.Boot
import spray.can.Http
import spray.http.HttpMethods._
import spray.http._

import scala.concurrent.{Await, Future, Promise}
import scala.util.Try


trait EveXmlApi[T] extends EveXmlParser with ResponseBodyDecoder {

  import Boot._

  type Type = T

  def uriPath: String

  def log: LoggingAdapter

  def parseResponseBody(uri: Uri, xml: String): T

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, Boot.ajaxFutureTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  def hostConnectorSetup = CharacterInfoRetriever.hostConnectorSetup

  def httpGetUri(query: Uri.Query): Uri = Uri(path = Uri.Path(uriPath), query = query)

  def complete(uri: Uri): Future[T] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val started = System.currentTimeMillis()
    // use the rest timeout here - this is where we want to fail so the incomplete answer logic works
    val httpFuture = ask(hostConnector, HttpRequest(GET, uri))(Boot.restTimeout.duration)
    val promise = Promise[T]()
    httpFuture onSuccess {
      case resp: HttpResponse =>
        if (resp.status.isSuccess) {
          log.debug("http get: {} ===> {} in {}ms", uri, resp.status.intValue, System.currentTimeMillis - started)
          promise.complete(Try(parseResponseBody(uri, decodeResponseBody(resp))))
        } else {
          log.debug("http get error: {} {} after {}ms", resp.status.intValue, resp.entity.data.asString,
            System.currentTimeMillis - started)
          promise.failure(new IllegalArgumentException("http result not ok: " + resp.status.intValue))
        }
    }
    promise.future
  }

}

