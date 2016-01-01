package me.rjfarmer.rlh.client

// for correct macro application
import autowire._
import me.rjfarmer.rlh.api.{CharInfo, Api}
import me.rjfarmer.rlh.logging.LoggerRLH
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

@JSExport
object LittleHelper {

  val log = LoggerRLH("client.LittleHelper")

  val pilotBox = textarea(cols:=20, rows:=10).render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  val pilotList = tbody().render

  def clearLogButtonClick(ev: dom.Event): Unit = {
    dom.document.getElementById("logMessages").innerHTML = ""
  }

  def stringify(obj: js.Any): String = js.Dynamic.global.JSON.stringify(obj).asInstanceOf[String]

  def menuClick(ev: dom.Event): Unit = {
    try {
      ev.stopPropagation()
      val myA = ev.target.asInstanceOf[html.Anchor]
      val myLI = myA.parentNode.asInstanceOf[html.LI]
      val myUL = myLI.parentNode.asInstanceOf[html.UList]
      val linkTarget =  myA.href.replaceFirst(""".*\#""", "")
      // log.info("linkTarget: " + linkTarget)

      val children = myUL.children
      for (i <- 0 until children.length) {
        // log.info("removing pure-menu-selected:", children.item(i))
        children.item(i).setAttribute("class", "pure-menu-item")
      }
      // log.info("adding pure-menu-selected", myLI)
      myLI.setAttribute("class", "pure-menu-item pure-menu-selected")

      for (theId <- Seq("rlhMain", "rlhLogging")) {
        dom.document.getElementById(theId).setAttribute("hidden", "hidden")
      }
      dom.document.getElementById(linkTarget).removeAttribute("hidden")
    } catch {
      case ex: Exception => log.error("Exception:", ex)
    }
  }

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val pilotNames = pilotBox.value.split("""\n""")
    log.debug("calling listCharacters with " + pilotNames.size + " pilots")
    val future = Ajaxer[Api].listCharacters(pilotNames).call()
    future.onFailure { case ex: Throwable => dom.alert("Error: " + ex) }
    future.foreach { pilots =>
      log.info("listCharacters: received " + pilots.size + " pilots")
      pilotList.innerHTML = ""
      for (p <- pilots) {
        val allianceOrCorp: String = p.info.alliance getOrElse { p.info.corporation }
        val trow = tr(id:=p.info.characterID,
          td( a(href:=zkillboardUrl(p), target:="_blank", p.info.characterName)),
          td(allianceOrCorp),
          td(p.info.characterAge),
          td(p.zkStats.activepvp.kills),
          td(p.zkStats.lastMonths.shipsDestroyed + "/" + p.zkStats.lastMonths.shipsLost)
          ).render
        pilotList.appendChild(trow)
      }
    }
  }

  def zkillboardUrl(p: CharInfo): String = {
    s"""https://zkillboard.com/character/${p.info.characterID}/"""
  }

  @JSExport
  def main(container: html.Div) = {
    log.info("LittleHelper.main called")
    dom.document.getElementById("clearLogButton").asInstanceOf[html.Button].onclick = clearLogButtonClick _
    dom.document.getElementById("rlhMenu").asInstanceOf[html.Span].onclick = menuClick _

    container.appendChild(
        div(cls:="pure-g",
          form(cls:="pure-u-1-3 pure-form pure-form-stacked",
            onsubmit := formSubmit _,
            pilotBox,
            button(cls:="pure-button pure-button-primary", `type`:="submit", "Submit")),
          div(cls:="pure-u-2-3",
            table(cls:="pure-table pure-table-striped",
              thead(tr(th("Name"), th("Alliance/Corp"), th("Age"), th("Act. Kills"), th("2 Months"))),
            pilotList))
      ).render
    )
  }

}
