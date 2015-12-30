package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.{ZKillBoardApi, EveXmlApi}
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.{Failure, Success}


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

  override def listCharacters(names: Seq[String]): Future[Seq[CharInfo]] = {
    val idsFuture = EveXmlApi.listCharacters(names)
    val result = Promise[Seq[CharInfo]]()
    idsFuture.onComplete {
      case Success(ids) =>
        val f1 = Future.sequence(ids.map((ian) => EveXmlApi.characterInfo(ian.characterID)))
        val f2 = Future.sequence(ids.map((ian) => ZKillBoardApi.zkStats(ian.characterID.toLong)))
        f1.zip(f2).onComplete {
          case Success(Pair(infos, zkstats)) =>
            result.success(infos.zip(zkstats).map { pair => CharInfo(pair._1, pair._2)})
          case Failure(ex) =>
            result.failure(ex)
        }
      case Failure(ex) =>
        result.failure(ex)
    }
    result.future
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



