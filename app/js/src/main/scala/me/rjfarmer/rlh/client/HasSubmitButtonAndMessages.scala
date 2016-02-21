package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.logging.LoggerRLH
import org.scalajs.dom

import scalatags.JsDom.all._

/**
 * A submit button it has!
 *
 * Also an area for messages!
 *
 */
trait HasSubmitButtonAndMessages {

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

  val submitButton = button(cls := "pure-button pure-button-primary", `type` := "submit", "Submit").render

  val messageBox = div(hidden).render


  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    formSubmitAction(ev, tsStarted)
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
