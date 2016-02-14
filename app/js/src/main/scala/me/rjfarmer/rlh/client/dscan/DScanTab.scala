package me.rjfarmer.rlh.client.dscan

// for correct macro appliction
import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.client.{Message, Ajaxer, TabbedPanel}
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{DScanLineCheck, SharedConfig}
import org.scalajs.dom

import scalatags.JsDom.all._

object DScanTab extends TabbedPanel {

  import me.rjfarmer.rlh.client.PimpedDomElement._
  import scala.concurrent.ExecutionContext.Implicits.global

  override val panelName = "D-Scan"

  private val dscanBox = textarea(cols := 20, rows := 10,
    placeholder := "Paste EVE D-Scan").render
  dscanBox.onfocus = (ev: dom.Event) => dscanBox.value = ""

  private val messageBox = div(hidden).render

  private val submitButton = button(cls := "pure-button pure-button-primary", `type` := "submit", "Submit").render

  override val panelView = div(id := "dscanTab",
    cls := "pure-g",
    form(cls := "pure-u-1-3 pure-form pure-form-stacked",
      onsubmit := formSubmit _,
      dscanBox,
      submitButton),
    div(cls := "pure-u-2-3",
      messageBox,
      h1(DScanDetailsView.dscanItemCount, DScanDetailsView.solarSystem, DScanDetailsView.respTimeAgo),
      table(cls := "pure-table pure-table-striped",
        thead(tr(th("Name"), th("Type"), th("Group"), th("Category"), th("Distance"))),
        DScanDetailsView.dscanList)
    )
  ).render


  private val log = LoggerRLH("client.local.DScanTab")

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    val dscanLines = dscanBox.value.split( """\n""")
    val illegalLines = dscanLines.filterNot(DScanLineCheck.isValidDScanLine)
    if (illegalLines.nonEmpty) {
      log.info("illegal dscan lines: " + illegalLines.mkString(", "))
    }

    val validLines = dscanLines.filter(DScanLineCheck.isValidDScanLine).toVector

    log.debug("calling parseDScan with " + validLines.length + " lines")
    val req = DScanParseRequest(SharedConfig.client.clientSoftwareVersion, validLines, "", None, None)
    val future= Ajaxer[Api].parseDScan(req).call()
    future.onFailure { case ex: Throwable =>
      submitError(tsStarted, ex)
    }
    future.foreach { response => submitSuccess(tsStarted, response) }
  }

  // XXX extract me
  def submitStarted: Long = {
    submitButton.addClass("pure-button-disabled")
    System.currentTimeMillis()
  }

  // XXX extract me
  def submitSuccess(started: Long, resp: DScanParseResponse): Unit = {
    val now = System.currentTimeMillis
    val lines = resp.lines

    log.info("parseDScan: received " + lines.size + " dscan lines in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")

    submitButton.removeClass("pure-button-disabled")

    handleMessageBox(messages(resp))

    DScanDetailsView.update(resp)
  }

  def handleMessageBox(messages: Seq[Message]): Unit = {
    messages match {
      case Seq() =>
        messageBox.setAttribute("hidden", "hidden")
        messageBox.innerHTML = ""
      case _ =>
        messageBox.innerHTML = ""
        messages.foreach { msg =>
          messageBox.appendChild(div(`class` := msg.msgType.messageClass, msg.msg).render)
        }
        messageBox.removeAttribute("hidden")
    }
  }

  def submitError(started: Long, ex: Throwable): Unit = {
    val now = System.currentTimeMillis
    log.error("Ajax Exception after " + (now - started) + "ms: " + ex)
    submitButton.removeClass("pure-button-disabled")

    handleMessageBox(Seq(Message.error("Error communicating with the server")))
  }

  // XXX extract me
  def messages(resp: DScanParseResponse): Seq[Message] = {
    resp.message.map(Message.error).toSeq
  }



}
