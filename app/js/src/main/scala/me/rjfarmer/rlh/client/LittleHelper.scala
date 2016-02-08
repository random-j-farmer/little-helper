package me.rjfarmer.rlh.client

// for correct macro application

import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{SharedConfig, ClientConfig, EveCharacterName}
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

object Ajaxer extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  override def doCall(req: Request) = {
    Ajax.post(url = "/ajax/" + req.path.mkString("/"), data=upickle.default.write(req.args)).map(_.responseText)
  }

  def read[Result: upickle.default.Reader](p:String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

class LittleHelper {

  import PimpedDomElement._

  val log = LoggerRLH("client.LittleHelper")

  val pilotBox = textarea(cols := 20, rows := 10).render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  val messageBox = div(hidden, `class` := "info").render

  val submitButton = button(cls := "pure-button pure-button-primary", `type` := "submit", "Submit").render

  var listCharactersView = new ListCharactersView()

  def clearLogButtonClick(ev: dom.Event): Unit = {
    dom.document.getElementById("logMessages").innerHTML = ""
  }

  // def stringify(obj: js.Any): String = js.Dynamic.global.JSON.stringify(obj).asInstanceOf[String]

  def menuClick(ev: dom.Event): Unit = {
    try {
      ev.stopPropagation()
      val myA = ev.target.asInstanceOf[html.Anchor]
      val myLI = myA.parentNode.asInstanceOf[html.LI]
      val myUL = myLI.parentNode.asInstanceOf[html.UList]
      val linkTarget = myA.href.replaceFirst( """.*\#""", "")
      // log.info("linkTarget: " + linkTarget)

      val children = myUL.children
      for (i <- 0 until children.length) {
        // log.info("removing pure-menu-selected:", children.item(i))
        children.item(i).setAttribute("class", "pure-menu-item")
      }
      // log.info("adding pure-menu-selected", myLI)
      myLI.setAttribute("class", "pure-menu-item pure-menu-selected")

      for (theId <- Seq("localTab", "loggingTab")) {
        dom.document.getElementById(theId).setAttribute("hidden", "hidden")
      }
      dom.document.getElementById(linkTarget).removeAttribute("hidden")
    } catch {
      case ex: Exception => log.error("Exception:", ex)
    }
  }

  def submitStarted: Long = {
    submitButton.addClass("pure-button-disabled")
    System.currentTimeMillis()
  }

  def submitSuccess(started: Long, resp: ListCharactersResponse): Unit = {
    val now = System.currentTimeMillis
    val pilots = resp.charinfos

    log.info("listCharacters: received " + pilots.size + " pilots in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")

    submitButton.removeClass("pure-button-disabled")

    handleMessageBox(resp.message)

    listCharactersView.update(resp)
  }

  def handleMessageBox(msg: Option[String]): Unit = {
    msg match {
      case None =>
        messageBox.setAttribute("hidden", "hidden")
        messageBox.innerHTML = ""
      case Some(txt) =>
        messageBox.innerHTML = ""
        messageBox.appendChild(span(txt).render)
        messageBox.removeAttribute("hidden")
    }
  }

  def submitError(started: Long, ex: Throwable): Unit = {
    val now = System.currentTimeMillis
    log.error("Ajax Exception after " + (now - started) + "ms: " + ex)
    submitButton.removeClass("pure-button-disabled")

    handleMessageBox(Some("Error communicating with the server"))
  }

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    val pilotNames = pilotBox.value.split( """\n""").map(_.trim)
    val illegalNames = pilotNames.filterNot(EveCharacterName.isValidCharacterName)
    if (illegalNames.nonEmpty) {
      log.info("illegal pilot names: " + illegalNames.mkString(", "))
    }

    val validNames = pilotNames.filter(EveCharacterName.isValidCharacterName).toVector

    log.debug("calling listCharacters with " + validNames.length + " pilots")
    val req = ListCharactersRequest(SharedConfig.client.clientSoftwareVersion, validNames, "", None, None)
    val future = Ajaxer[Api].listCharacters(req).call()
    future.onFailure { case ex: Throwable =>
      submitError(tsStarted, ex)
    }
    future.foreach { response => submitSuccess(tsStarted, response) }
  }

  def main(_body: html.Body) = {

    val rlhMain = div(id := "rlhMain",
      div(id := "rlhMenu", cls := "pure-menu pure-menu-horizontal", onclick := menuClick _,
        style := "display: inline; position: absolute; top: 0px; right: 0px; width: 5cm; text-align: right;",
        ul(cls := "pure-menu-list",
          li(cls := "pure-menu-item pure-menu-selected", a(href := "#localTab", cls := "pure-menu-link", "Local")),
          li(cls := "pure-menu-item", a(href := "#loggingTab", cls := "pure-menu-link", "Logging"))
        )
      ),
      div(id := "localTab",
        cls:="pure-g",
        form(cls:="pure-u-1-3 pure-form pure-form-stacked",
          onsubmit := formSubmit _,
          pilotBox,
          submitButton),
        div(cls:="pure-u-2-3",
          messageBox,
          h1(listCharactersView.pilotCount, listCharactersView.solarSystem, listCharactersView.respTimeAgo),
          h2("Pilots by Alliance/Corp"),
          table(cls:="pure-table pure-table-striped",
            thead(tr(th("Alliance/Corp"), th("# Pilots"))),
            listCharactersView.corpList),
          h2("Pilots"),
          table(cls:="pure-table pure-table-striped",
            thead(tr(th("Name"), th("Alliance/Corp"), th("Kills/Deaths"), th("Age"))),
            listCharactersView.pilotList)
        )
      ),
      div(id := "loggingTab", hidden,
        h2("Log Messages"),
        button(id := "clearLogButton", cls := "pure-button pure-button-primary",
          `type` := "button", "Clear Log", onclick := clearLogButtonClick _),
        br(),
        table(cls := "pure-table pure-table-striped", width := "100%",
          col(width := "10%"), col(width := "10%"), col(width := "80%"),
          thead(
            th("ms"), th("Level"), th("Message")),
          tbody(id := "logMessages")
        )
      )
    ).render

    _body.appendChild(rlhMain)

    dom.window.setInterval(listCharactersView.refreshResponseTimeAgo _, 10000d)

    log.info("LittleHelper.main: " + SharedConfig.client)
  }


}

@JSExport
object LittleHelper {

  @JSExport
  def main(_body: html.Body, configJson: js.Any) = {
    val clientConfig = upickle.default.readJs[ClientConfig](upickle.json.readJs(configJson))
    SharedConfig.client = clientConfig
    val lh = new LittleHelper()
    lh.main(_body)
  }

}

