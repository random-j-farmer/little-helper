package me.rjfarmer.rlh.eve

import java.text.SimpleDateFormat
import java.util.{TimeZone, Date}

import akka.actor.ActorRef
import akka.event.{LoggingAdapter, Logging}
import akka.io.IO
import akka.routing.FromConfig
import akka.pattern.ask
import me.rjfarmer.rlh.server.Boot
import me.rjfarmer.rlh.server.Boot._
import spray.can.Http
import spray.http.HttpMethods._
import spray.http.{HttpResponse, HttpRequest, Uri}

import scala.concurrent.{Promise, Future, Await}
import scala.util.Try
import scala.xml.Node


trait EveXmlApi[T] {

  type Type = T

  def uriPath: String

  def log: LoggingAdapter

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, bootTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  def hostConnectorSetup = Http.HostConnectorSetup("api.eveonline.com", port=443, sslEncryption = true)

  def successMessage(xml: String): T

  def httpGetUri(query: Uri.Query): Uri = Uri(path = Uri.Path(uriPath), query = query)

  def complete(query: Uri.Query): Future[T] = {
    import Boot._
    import scala.concurrent.ExecutionContext.Implicits.global
    val started = System.currentTimeMillis()
    val uri = httpGetUri(query)
    // log.debug("http get: {}", uri)
    val httpFuture = ask(hostConnector, HttpRequest(GET, httpGetUri(query)))
    val promise = Promise[T]()
    httpFuture onSuccess {
      case resp: HttpResponse =>
        if (resp.status.isSuccess) {
          log.debug("http get: {} ===> {} in {}ms", uri, resp.status.intValue, System.currentTimeMillis - started)
          promise.complete(Try(successMessage(resp.entity.data.asString)))
        } else {
          log.debug("http get error: {} {} after {}ms", resp.status.intValue, resp.entity.data.asString,
            System.currentTimeMillis - started)
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

  /**
   * Parses the date/time string as UTC date time.
   *
   * Example input: 2008-04-15 08:17:23
   * @param dt string input
   * @return
   */
  def parseDatetime(dt: String): Date = {
    val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    sdf.parse(dt)
  }


}
