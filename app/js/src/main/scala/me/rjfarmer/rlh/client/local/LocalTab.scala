package me.rjfarmer.rlh.client.local

// for correct macro application

import autowire._
import me.rjfarmer.rlh.api.{Api, ListCharactersRequest, ListCharactersResponse}
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.client.{Message, Ajaxer, PimpedDomElement, TabbedPanel}
import me.rjfarmer.rlh.shared.{EveCharacterName, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.html.Div

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scalatags.JsDom.all._


object LocalTab extends TabbedPanel {

  private val pilotBox = textarea(cols := 20, rows := 10,
    placeholder := "Paste EVE Local").render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  private val messageBox = div(hidden).render

  private val submitButton = button(cls := "pure-button pure-button-primary", `type` := "submit", "Submit").render

  override val panelName: String = "Local"

  override val panelView: Div = div(id := "localTab",
    cls := "pure-g",
    form(cls := "pure-u-1-3 pure-form pure-form-stacked",
      onsubmit := formSubmit _,
      pilotBox,
      submitButton),
    div(cls := "pure-u-2-3",
      messageBox,
      h1(ScanDetailsView.pilotCount, ScanDetailsView.solarSystem, ScanDetailsView.respTimeAgo),
      h2("Pilots by Alliance/Corp"),
      table(cls := "pure-table pure-table-striped",
        thead(tr(th("Alliance/Corp"), th("# Pilots"))),
        ScanDetailsView.corpList),
      h2("Pilots"),
      table(cls := "pure-table pure-table-striped",
        thead(tr(th("Name"), th("Alliance/Corp"), th("Kills/Deaths"), th("Age"))),
        ScanDetailsView.pilotList)
    )
  ).render

  import PimpedDomElement._

  private val log = LoggerRLH("client.local.LocalTab")

  def submitStarted: Long = {
    submitButton.addClass("pure-button-disabled")
    System.currentTimeMillis()
  }

  def submitSuccess(started: Long, resp: ListCharactersResponse): Unit = {
    val now = System.currentTimeMillis
    val pilots = resp.charinfos

    log.info("listCharacters: received " + pilots.size + " pilots in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")

    submitButton.removeClass("pure-button-disabled")

    handleMessageBox(messages(resp))

    ScanDetailsView.update(resp)
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

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    val pilotNames = pilotBox.value.split( """\n""").map(_.trim)
    val illegalNames = pilotNames.filterNot(EveCharacterName.isValidCharacterName)
    if (illegalNames.nonEmpty) {
      log.info("illegal pilot names: " + illegalNames.mkString(", "))
    }

    val validNames = pilotNames.filter(EveCharacterName.isValidCharacterName).toVector

    log.debug("calling listCharacters with " + validNames.length + " pilots")
    val req = ListCharactersRequest(SharedConfig.client.clientSoftwareVersion, validNames, "", None, None)
    val future = Ajaxer[Api].listCharacters(req).call()
    future.onFailure { case ex: Throwable =>
      submitError(tsStarted, ex)
    }
    future.foreach { response => submitSuccess(tsStarted, response) }
  }

  def messages(resp: ListCharactersResponse): Seq[Message] = {
    val (complete, incomplete) = resp.charinfos.partition(_.complete)
    val completeButStale = complete.filterNot(_.isFresh)
    val msgSuffix = " in the background and will be ready for your next request."
    val incompleteMsg: Option[Message] = if (incomplete.isEmpty) None
      else Some(Message.warning(s"${incomplete.size} incomplete results.  The missing results will be retrieved and cached" + msgSuffix))
    val staleMsg: Option[Message] = if (completeButStale.isEmpty) None
      else Some(Message.info(s"${completeButStale.size} stale responses.  The stale results will be refreshed" + msgSuffix))
    Seq() ++ resp.message.map(Message.error) ++ incompleteMsg ++ staleMsg
  }


}
