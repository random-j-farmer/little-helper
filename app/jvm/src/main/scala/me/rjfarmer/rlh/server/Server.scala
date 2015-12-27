package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.EveXmlApi
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {
  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read(p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

object Server extends SimpleRoutingApp with Api with RequestTimeout {

  def main(args: Array[String]): Unit = {
    import Boot._


    startServer(bootHost, port = bootPort) {
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

  override def listCharacters(names: Seq[String]): Future[Seq[CharacterInfo]] = {
    val idsFuture = EveXmlApi.listCharacters(names)
    idsFuture.flatMap(ids => Future.sequence(ids.map((ian) => EveXmlApi.characterInfo(ian.characterID))))
  }
}

object Boot extends RequestTimeout {

  val bootConfig = ConfigFactory.load()
  val bootHost = bootConfig.getString("http.host")
  val bootPort = bootConfig.getInt("http.port")

  implicit val bootSystem = ActorSystem("little-helper", bootConfig)
  implicit val bootTimeout = requestTimeout(bootConfig)

}

trait RequestTimeout {
  import scala.concurrent.duration._

  def requestTimeout(config: Config): Timeout = {
    val t = config.getString("spray.can.server.request-timeout")
    val d = Duration(t)
    FiniteDuration(d.length, d.unit)
  }
}



