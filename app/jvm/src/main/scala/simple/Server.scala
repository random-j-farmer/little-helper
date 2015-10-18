package simple

import akka.actor.ActorSystem
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Properties

object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {
  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read(p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

object Server extends SimpleRoutingApp with Api {

  def main(args: Array[String]): Unit = {
    implicit val system = ActorSystem()
    val port = Properties.envOrElse("PORT", "8080").toInt

    startServer("0.0.0.0", port = port) {
      get {
        pathSingleSlash {
          complete {
            HttpEntity(MediaTypes.`text/html`, Page.skeleton.render)
          }
        } ~
        getFromResourceDirectory("")
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

  def list(path: String) = {
    val (dir, last) = path.splitAt(path.lastIndexOf("/") + 1)
    val files = Option(new java.io.File("./" + dir).listFiles())
      .toSeq.flatten
    for {
      f <- files
      if f.getName.startsWith(last)
    } yield FileData(f.getName, f.length())
  }
}
