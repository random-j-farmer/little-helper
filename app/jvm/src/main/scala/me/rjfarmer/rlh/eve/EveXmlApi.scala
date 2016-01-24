package me.rjfarmer.rlh.eve

import java.text.SimpleDateFormat
import java.util.{Date, TimeZone}

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import akka.io.IO
import akka.pattern.ask
import me.rjfarmer.rlh.server.Boot
import me.rjfarmer.rlh.server.Boot._
import spray.can.Http
import spray.http.HttpHeaders.RawHeader
import spray.http.HttpMethods._
import spray.http._
import spray.httpx.encoding.{Gzip, Deflate}

import scala.concurrent.{Await, Future, Promise}
import scala.util.Try
import scala.xml.Node


trait EveXmlApi[T] {

  type Type = T

  def uriPath: String

  def log: LoggingAdapter

  def parseResponseBody(uri: Uri, xml: String): T

  def hostConnector: ActorRef = Await.result(IO(Http) ? hostConnectorSetup, bootTimeout.duration)
    .asInstanceOf[Http.HostConnectorInfo]
    .hostConnector

  val defaultHeaders: List[HttpHeader] =  if (Boot.bootConfig.getBoolean("little-helper.xml-api.use-compression")) {
    List(RawHeader("accept-encoding", "gzip,deflate"))
  } else {
    List()
  }
  def hostConnectorSetup = Http.HostConnectorSetup("api.eveonline.com", port=443, sslEncryption = true,
    defaultHeaders = defaultHeaders)

  def httpGetUri(query: Uri.Query): Uri = Uri(path = Uri.Path(uriPath), query = query)

  def complete(uri: Uri): Future[T] = {
    import Boot._

    import scala.concurrent.ExecutionContext.Implicits.global
    val started = System.currentTimeMillis()
    // log.debug("http get: {}", uri)
    val httpFuture = ask(hostConnector, HttpRequest(GET, uri))
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

  def decodeResponseBody(resp: HttpResponse): String = {
    resp.encoding match {
      case HttpEncoding("gzip") =>
        Gzip.decode(resp).entity.asString
      case HttpEncoding("deflate") =>
        Deflate.decode(resp).entity.asString
      case _ =>
        resp.entity.asString
    }
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
