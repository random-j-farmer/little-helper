package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.dscan.{DScanDetailsView, DScanTab}
import me.rjfarmer.rlh.client.local.{LocalTab, LocalDetailsView}
import me.rjfarmer.rlh.client.logging.{LoggerRLH, LoggingTab}
import me.rjfarmer.rlh.shared.{ClientConfig, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object LittleHelper {

  val log = LoggerRLH("client.LittleHelper")

  val tabPanel = TabPanel(Vector(LocalTab, DScanTab, LoggingTab))

  val mainView = div(id := "rlhMain",
    tabPanel.linksView,
    tabPanel.panelView
  ).render

  private[this] val FragmentPattern = """^#([\w/]*)$""".r

  private[this] var locationFrag: String = ""

  /** set the location fragment (starting with #) for client side routing */
  def setLocationFragment(frag: String): Unit = {
    locationFrag = frag
    // does not work in IGB, so we set a var as well
    js.Dynamic.global.location.hash = locationFrag
  }

  def getLocationUrl: String = getLocationUrl(locationFrag)

  def getLocationUrl(frag: String): String = {
    val url = js.Dynamic.global.location.href.asInstanceOf[String].replaceAll("#.*".r.regex, "")
    url + frag
  }

  @JSExport
  def main(_body: html.Body, configJson: js.Any) = {
    val clientConfig = upickle.default.readJs[ClientConfig](upickle.json.readJs(configJson))
    SharedConfig.client = clientConfig

    _body.appendChild(mainView)

    // we cant log before the log div is added to the body
    log.info("LittleHelper.main: " + SharedConfig.client)

    // the fragment in the browser url
    val currentFragment = Option(js.Dynamic.global.location.hash.asInstanceOf[String])
    currentFragment match {
      case Some(FragmentPattern(panelId)) =>
        log.debug("document fragment: current panel id: " + panelId)
        val path = panelId.split('/')
        tabPanel.route(path)
        setLocationFragment("#" + panelId)
      case _ =>
        log.debug("no fragment to route:" + currentFragment)
    }


    dom.window.setInterval(LocalTab.refreshResponseTimeAgo _, 10000d)
    dom.window.setInterval(LocalDetailsView.refreshResponseTimeAgo _, 10000d)
    dom.window.setInterval(DScanDetailsView.refreshResponseTimeAgo _, 10000d)
  }

}

