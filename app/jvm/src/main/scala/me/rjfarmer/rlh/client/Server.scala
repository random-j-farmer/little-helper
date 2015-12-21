package me.rjfarmer.rlh.client

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{ConfigFactory, Config}
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global

object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {
  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read(p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

object Server extends SimpleRoutingApp with RequestTimeout with Api {

  def main(args: Array[String]): Unit = {
    val config = ConfigFactory.load()
    val host = config.getString("http.host")
    val port = config.getInt("http.port")

    implicit val system = ActorSystem("little-helper", config)
    implicit val timeout = requestTimeout(config)

    startServer("0.0.0.0", port = port) {
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
          extract(_.request.entity.asString) { e =>
            complete(Router.route[Api](Server)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](e))))
          }
        }
      }
    }
  }

  def listPilots(pilots: Array[String]): Seq[PilotInfo] = {
    for (p <- pilots)
      yield PilotInfo("666", p, "Eve Scout")
  }

}

trait RequestTimeout {
  import scala.concurrent.duration._

  def requestTimeout(config: Config): Timeout = {
    val t = config.getString("spray.can.server.request-timeout")
    val d = Duration(t)
    FiniteDuration(d.length, d.unit)
  }
}
