package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.dscan.DScanTab
import me.rjfarmer.rlh.client.local.LocalTab
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{DScanLineCheck, EveCharacterName}
import org.scalajs.dom

import scalatags.JsDom.all._

/**
 *
 * Handles a smart paste form.
 *
 * It has a textarea for cut and pasting, a submit button and a message display area.
 *
 * The submit action looks at the content of the paste box and switches to the
 * correct tab if necessary.
 *
 */
trait SmartPasteSubmit {

  /**
   * Logger to use in common methods.
   *
   * @return the logger
   */
  def log: LoggerRLH

  /**
   * Callback for form submit.
   *
   * submit button handling is done by formSubmit/submitStarted/submitFinished,
   * just do the logic here.
   *
   * @param ev event
   * @param tsStarted timestamp
   */
  def formSubmitAction(ev: dom.Event, tsStarted: Long): Unit


  import me.rjfarmer.rlh.client.PimpedDomElement._

  val pasteBox = textarea(cols := 25, rows := 10,
    placeholder := "Paste EVE Local/DScan").render
  pasteBox.onfocus = (ev: dom.Event) => pasteBox.value = ""


  val submitButton = button(cls := "pure-button pure-button-primary", `type` := "submit", "Submit").render

  val messageBox = div(hidden).render


  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    // this is the "smart" part
    // depending on input type, it tries to route to the correct form
    val lines = pasteBox.value.split( """\n""").map(_.trim)
    if (lines.forall(EveCharacterName.isValidCharacterName)) {
      if (pasteBox == LocalTab.pasteBox) {
        formSubmitAction(ev, tsStarted)
      } else {
        LittleHelper.tabPanel.activatePanel("localTab")
        LocalTab.pasteBox.value = pasteBox.value
        LocalTab.formSubmitAction(ev, tsStarted)
      }
    } else if (lines.forall(DScanLineCheck.isValidDScanLine)) {
      if (pasteBox == DScanTab.pasteBox) {
        formSubmitAction(ev, tsStarted)
      } else {
        LittleHelper.tabPanel.activatePanel("dscanTab")
        DScanTab.pasteBox.value = pasteBox.value
        DScanTab.formSubmitAction(ev, tsStarted)
      }
    } else {
      // fallback: use the current form
      formSubmitAction(ev, tsStarted)
    }
  }

  def submitStarted: Long = {
    submitButton.addClass("pure-button-disabled")
    System.currentTimeMillis()
  }

  def submitError(started: Long, ex: Throwable): Unit = {
    val now = System.currentTimeMillis
    log.error("Ajax Exception after " + (now - started) + "ms: " + ex)
    submitFinished(started, Seq(Message.error("Error communicating with the server")))
  }

  def submitFinished(started: Long, messages: Seq[Message]): Unit = {
    submitButton.removeClass("pure-button-disabled")
    handleMessageBox(messages)
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


}
