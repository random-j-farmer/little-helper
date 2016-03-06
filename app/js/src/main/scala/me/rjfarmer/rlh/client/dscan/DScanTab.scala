package me.rjfarmer.rlh.client.dscan

// for correct macro appliction

import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.client._
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{DScanLineCheck, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.raw.HTMLLIElement

import scala.scalajs.js
import scala.util.{Failure, Success}
import scalatags.JsDom.all._

object DScanTab extends TabbedPanel with HistoryPanel[DScanParseResponse] with SmartPasteSubmit {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val panelName = "D-Scan"

  override val history = new History[DScanParseResponse]()

  override val panelView = div(cls := "pure-g",
    div(cls := "pure-u-1-3",

      form(cls := "pure-form pure-form-stacked",
        onsubmit := formSubmit _,
        pasteBox,
        submitButton),

      div(cls := "pure-menu restricted-width",
        historyView
      )
    ),
    div(cls := "pure-u-2-3",
      messageBox,
      h1(DScanDetailsView.dscanItemCount, DScanDetailsView.solarSystem,
        DScanDetailsView.respTimeAgo),

      table(cls := "pure-table dscan-tree",
        caption(DScanDetailsView.nearestCelestial),
        thead(tr(th(`class` := "name-col", "Type/Name"), th(`class` := "dist-col", "Distance"))),
        DScanDetailsView.dscanList),

      p(),
      form(cls := "pure-form",
        fieldset(
          legend("Share result URL"),
          DScanDetailsView.resultUrlBox)))
  ).render

  override val log = LoggerRLH("client.dscan.DScanTab")


  def submitSuccess(started: Long, resp: DScanParseResponse): Unit = {
    val now = System.currentTimeMillis
    val lines = resp.lines

    log.info("parseDScan: received " + lines.size + " dscan lines in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")
    LittleHelper.refreshJsonWebToken(resp.refreshedJsonWebToken)
    DScanDetailsView.update(resp)
    addResultToHistory(resp, resp.lines.isEmpty)
    submitFinished(started, messages(resp))
  }

  override def formSubmitAction(ev: dom.Event, started: Long): Unit = {
    val dscanLines = pasteBox.value.split( """\n""")
    val illegalLines = dscanLines.filterNot(DScanLineCheck.isValidDScanLine)
    if (illegalLines.nonEmpty) {
      log.info("illegal dscan lines: " + illegalLines.mkString(", "))
    }

    val validLines = dscanLines.filter(DScanLineCheck.isValidDScanLine).toVector

    log.debug("calling parseDScan with " + validLines.length + " lines")
    val req = DScanParseRequest(SharedConfig.client.clientSoftwareVersion, validLines)
    val future = Ajaxer[Api].parseDScan(req).call()

    future.onComplete {
      case Failure(ex) =>
        submitError(started, ex)

      case Success(resp) =>
        submitSuccess(started, resp)
    }
  }

  def shareUrl(ev: dom.Event): Unit = {
    js.Dynamic.global.prompt("Ctrl/Cmd-C:", LittleHelper.getLocationUrl)
  }

  def messages(resp: DScanParseResponse): Seq[Message] = {
    resp.message.map(Message.error).toSeq
  }

  override def route(args: Seq[String]): Unit = {
    args match {
      case Seq(cachedKey) =>
        val req = CachedDScanRequest(SharedConfig.client.clientSoftwareVersion, cachedKey)
        Ajaxer[Api].cachedDScan(req).call().onSuccess {
          case None =>
            log.info("route: cached result does not exist: " + cachedKey)
          case Some(resp) =>
            log.info("route: retrieved cached dscan result: " + resp.lines.length)
            DScanDetailsView.update(resp)
        }
    }
  }

  override def panelID: String= "dscanTab"

  /** get the panels fragment - changed by route! */
  override def urlFragment: String = (Vector("#" + panelID) ++ DScanDetailsView.resultCacheKey).mkString("/")


  //
  // history panel
  //
  override def historyItemView(hi: HistoryItem[DScanParseResponse]): HTMLLIElement = {
    li(cls := "pure-menu-item reset-menu-height",
      a(cls := "pure-menu-link",
        href := s"#dscanTab/${hi.item.cacheKey.get}",
        onclick := onHistoryClick(hi) _,
        span(s"${hi.item.lines.length} objects, "),
        span(hi.item.solarSystem.fold("")(ss => s"$ss, ")),
        hi.respTimeAgo
      )).render
  }

  override def updateDetailsView(resp: DScanParseResponse): Unit = {
    DScanDetailsView.update(resp)
  }


}
