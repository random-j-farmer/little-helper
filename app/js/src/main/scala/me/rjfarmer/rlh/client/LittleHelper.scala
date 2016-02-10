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

  val tabPanel = TabPanel(Vector(LocalTab, LoggingTab))

  def main(_body: html.Body) = {

    val rlhMain = div(id := "rlhMain",
      tabPanel.linksView,
      tabPanel.panelView
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

