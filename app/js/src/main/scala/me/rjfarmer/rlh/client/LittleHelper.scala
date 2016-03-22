package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.dscan.{DScanDetailsView, DScanTab}
import me.rjfarmer.rlh.client.local.{LocalDetailsView, LocalTab}
import me.rjfarmer.rlh.client.logging.{LoggerRLH, LoggingTab}
import me.rjfarmer.rlh.shared.{ClientConfig, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Span

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

@JSExport
object LittleHelper {

  val log = LoggerRLH("client.LittleHelper")

  val tabPanel = TabPanel(Vector(LocalTab, DScanTab, LoggingTab))

  // need the lazy because loginLink accesses the shared client config which is only set when main is executed
  lazy val mainView = div(id := "rlhMain",
    div(id := "navbar",
      style := "display: inline; position: absolute; top: 0px; right: 0px; width: 10cm; text-align: right;",
      loginLink,
      tabPanel.linksView),
    tabPanel.panelView
  ).render

  private[this] val FragmentPattern = """^#([\w/]*)$""".r

  private[this] var locationFrag: String = ""


  private def loginLink: Span = {
    SharedConfig.client.crestConfig match {
      case None =>
        span().render

      case Some(cc) =>

        SharedConfig.jsonWebToken match {

          case None =>
            val url = Vector[String]("https://login.eveonline.com/oauth/authorize?response_type=code",
              s"redirect_uri=${cc.redirectUrl}",
              s"client_id=${cc.clientID}",
              "scope=characterLocationRead")
            span(a(cls := "loginLogout", href := url.mkString("&"), "Login")).render

          case Some(jwt) =>
            span(a(cls := "loginLogout", href := "#", onclick := logoutClicked _, "Logout")).render
        }
    }
  }

  private def logoutClicked(ev: dom.Event): Unit = {
    ev.preventDefault()
    ev.stopPropagation()
    dom.document.location.assign("/")
  }

  /** set the location fragment (starting with #) for client side routing */
  def setLocationFragment(frag: String): Unit = {
    locationFrag = frag
    // we want only the tab in the browsers url, no request cache id
    js.Dynamic.global.location.hash = locationFrag.replaceAll("/[^/]*$".r.regex, "")
  }

  def getLocationUrl: String = getLocationUrl(locationFrag)

  def getLocationUrl(frag: String): String = {
    val url = dom.document.location.href.replaceAll("#.*".r.regex, "")
    val path = dom.document.location.pathname
    url.substring(0, url.length - path.length) + "/" + frag
  }

  @JSExport
  def main(_body: html.Body, configJson: js.Any) = {
    try {
      SharedConfig.client = upickle.default.readJs[ClientConfig](upickle.json.readJs(configJson))
      SharedConfig.jsonWebToken = PimpedDomElement.cookieMap.get("jwt")
      _body.appendChild(mainView)

      // we cant log before the log div is added to the body
      log.info("LittleHelper.main: " + SharedConfig.client)
      log.debug("LittleHelper.main: json web token:" + SharedConfig.jsonWebToken)

      // the fragment in the browser url
      val currentFragment = Option(dom.document.location.hash)
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
      dom.window.setInterval(DScanTab.refreshResponseTimeAgo _, 10000d)
      dom.window.setInterval(LocalDetailsView.refreshResponseTimeAgo _, 10000d)
      dom.window.setInterval(DScanDetailsView.refreshResponseTimeAgo _, 10000d)

    } catch {
      case ex: Throwable =>
        log.error("Initialization error: " + ex)
    }
  }

}

