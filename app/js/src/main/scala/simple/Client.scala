package simple



import autowire._ // needed for corrected macro application

import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html

import scala.scalajs.js.annotation.JSExport

import scalajs.concurrent.JSExecutionContext.Implicits.runNow

import scalatags.JsDom.all._

object Ajaxer extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  override def doCall(req: Request) = {
    Ajax.post(url = "/ajax/" + req.path.mkString("/"), data=upickle.default.write(req.args)).map(_.responseText)
  }

  def read[Result: upickle.default.Reader](p:String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

@JSExport
object Client {
  @JSExport
  def main(container: html.Div) = {
    val inputBox = input.render
    val outputBox = ul.render
    def update() = {
      Ajaxer[Api].list(inputBox.value).call().foreach {data =>
        outputBox.innerHTML = ""
        for (FileData(name, size) <- data) {
          outputBox.appendChild(li(b(name), " - ", size, " bytes").render)
        }
      }
    }

    inputBox.onkeyup = (e: dom.Event) => update()

    update()

    container.appendChild(
      div(h1("File Search"),
        inputBox,
        outputBox).render
    )
  }
}
