package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.local.{LocalTab, ScanDetailsView}
import me.rjfarmer.rlh.client.logging.{LoggerRLH, LoggingTab}
import me.rjfarmer.rlh.shared.{ClientConfig, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

class LittleHelper {

  val log = LoggerRLH("client.LittleHelper")

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

  def main(_body: html.Body) = {

    val rlhMain = div(id := "rlhMain",
      div(id := "rlhMenu", cls := "pure-menu pure-menu-horizontal", onclick := menuClick _,
        style := "display: inline; position: absolute; top: 0px; right: 0px; width: 5cm; text-align: right;",
        ul(cls := "pure-menu-list",
          li(cls := "pure-menu-item pure-menu-selected", a(href := "#localTab", cls := "pure-menu-link", "Local")),
          li(cls := "pure-menu-item", a(href := "#loggingTab", cls := "pure-menu-link", "Logging"))
        )
      ),
      LocalTab.localTabView,
      LoggingTab.loggingTabView
    ).render

    _body.appendChild(rlhMain)

    dom.window.setInterval(ScanDetailsView.refreshResponseTimeAgo _, 10000d)

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

