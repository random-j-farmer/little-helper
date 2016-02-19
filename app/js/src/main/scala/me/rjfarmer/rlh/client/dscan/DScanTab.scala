package me.rjfarmer.rlh.client.dscan

// for correct macro appliction

import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.client._
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{DScanLineCheck, SharedConfig}
import org.scalajs.dom

import scala.util.{Failure, Success}
import scalatags.JsDom.all._

object DScanTab extends TabbedPanel with Submitable {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val panelName = "D-Scan"

  private val dscanBox = textarea(cols := 20, rows := 10,
    placeholder := "Paste EVE D-Scan").render
  dscanBox.onfocus = (ev: dom.Event) => dscanBox.value = ""

  override val panelView = div(id := "dscanTab",
    cls := "pure-g",
    form(cls := "pure-u-1-3 pure-form pure-form-stacked",
      onsubmit := formSubmit _,
      dscanBox,
      submitButton),
    div(cls := "pure-u-2-3",
      messageBox,
      h1(DScanDetailsView.dscanItemCount, DScanDetailsView.solarSystem, DScanDetailsView.respTimeAgo),
      table(cls := "pure-table dscan-tree",
        caption(DScanDetailsView.nearestCelestial),
        thead(tr(th(`class` := "name-col", "Type/Name"), th(`class` := "dist-col", "Distance"))),
        DScanDetailsView.dscanList)
    )
  ).render

  override val log = LoggerRLH("client.dscan.DScanTab")

  override def formSubmitAction(ev: dom.Event, started: Long): Unit = {
    val dscanLines = dscanBox.value.split( """\n""")
    val illegalLines = dscanLines.filterNot(DScanLineCheck.isValidDScanLine)
    if (illegalLines.nonEmpty) {
      log.info("illegal dscan lines: " + illegalLines.mkString(", "))
    }

    val validLines = dscanLines.filter(DScanLineCheck.isValidDScanLine).toVector

    log.debug("calling parseDScan with " + validLines.length + " lines")
    val req = DScanParseRequest(SharedConfig.client.clientSoftwareVersion, validLines, "", None, None)
    val future = Ajaxer[Api].parseDScan(req).call()

    future.onComplete {
      case Failure(ex) =>
        submitError(started, ex)

      case Success(resp) =>
        val now = System.currentTimeMillis
        val lines = resp.lines

        log.info("parseDScan: received " + lines.size + " dscan lines in " +
          resp.solarSystem + " in " +
          (now - started) + "ms")
        DScanDetailsView.update(resp)
        LittleHelper.setLocationFragment("dscanTab", resp.cacheKey.toSeq)

        submitFinished(started, messages(resp))

    }
  }

  def messages(resp: DScanParseResponse): Seq[Message] = {
    resp.message.map(Message.error).toSeq
  }

  override def route(args: Seq[String]): Unit = {
    args match {
      case Seq(cachedKey) =>
        val req = CachedDScanRequest(SharedConfig.client.clientSoftwareVersion, cachedKey, "", None, None)
        Ajaxer[Api].cachedDScan(req).call().onSuccess {
          case None =>
            log.info("route: cached result does not exist: " + cachedKey)
          case Some(resp) =>
            log.info("route: retrieved cached dscan result: " + resp.lines.length)
            DScanDetailsView.update(resp)
        }
    }
  }

}
