package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.shared.SharedConfig
import org.scalajs.dom.ext.Ajax
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object Ajaxer extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  override def doCall(req: Request) = {
    val headerMap = SharedConfig.jsonWebToken match {
      case None =>
        Map.empty[String, String]
      case Some(jwt) =>
        Map("Authorization" -> ("Bearer " + jwt))
    }
    Ajax.post(url = "/ajax/" + req.path.mkString("/"),
      headers = headerMap,
      data = upickle.default.write(req.args)).map(_.responseText)
  }

  def read[Result: upickle.default.Reader](p: String) = upickle.default.read[Result](p)

  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

