package me.rjfarmer.rlh.client.local

// for correct macro application

import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.client._
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.{EveCharacterName, SharedConfig}
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.HTMLLIElement

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js
import scala.util.{Failure, Success}
import scalatags.JsDom.all._

import PimpedDomElement._


object LocalTab extends TabbedPanel with HasSubmitButtonAndMessages {

  private val pilotBox = textarea(cols := 20, rows := 10,
    placeholder := "Paste EVE Local").render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  private val localHistory = new History[ListCharactersResponse]()
  private val localHistoryBox = ul(cls := "pure-menu-list").render

  override val panelName: String = "Local"

  override val panelView: Div = div(id := "localTab",
    cls := "pure-g",
    div(cls := "pure-u-1-3",

      form(cls := "pure-form pure-form-stacked",
        onsubmit := formSubmit _,
        pilotBox,
        submitButton),

      div(cls := "pure-menu restricted-width",
        localHistoryBox
      )
    ),
    div(cls := "pure-u-2-3",
      messageBox,
      h1(LocalDetailsView.pilotCount, LocalDetailsView.solarSystem, LocalDetailsView.respTimeAgo),
      h2("Pilots by Alliance/Corp"),
      table(cls := "pure-table pure-table-striped",
        thead(tr(th("Alliance/Corp"), th("# Pilots"))),
        LocalDetailsView.corpList),

      h2("Pilots by kills in the last 2 months"),
      table(cls := "pure-table pure-table-striped",
        thead(tr(th("Name"), th("Alliance/Corp"), th("Kills/Deaths"), th("Age"))),
        LocalDetailsView.pilotList),
      p(),

      form(cls := "pure-form",
        fieldset(
          legend("Share result URL"),
          LocalDetailsView.resultUrlBox)))
  ).render

  override val log = LoggerRLH("client.local.LocalTab")

  def submitSuccess(started: Long, resp: ListCharactersResponse): Unit = {
    val now = System.currentTimeMillis
    val pilots = resp.charinfos

    log.info("listCharacters: received " + pilots.size + " pilots in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")

    LocalDetailsView.update(resp)
    addResultToHistory(resp)
    submitFinished(started, messages(resp))
  }

  private def addResultToHistory(resp: ListCharactersResponse): Unit = {
    val hView = historyView(localHistory.add(resp))
    localHistoryBox.insertChild(hView)
    val hViewLink = hView.getElementsByTagName("A").item(0).asInstanceOf[html.Anchor]
    makeHistoryLinkActive(hViewLink)
  }

  private def makeHistoryLinkActive(link: html.Anchor): Unit = {
    val nodeList = localHistoryBox.getElementsByTagName("A")
    for (i <- 0 until nodeList.length) {
      nodeList.item(i).asInstanceOf[html.Anchor].removeClass("pure-menu-selected")
    }
    link.addClass("pure-menu-selected")
  }


  private def historyView(hi: HistoryItem[ListCharactersResponse]): HTMLLIElement = {
    li(cls := "pure-menu-item reset-menu-height",
      a(cls := "pure-menu-link",
        href := s"#localTab/${hi.item.cacheKey.get}",
      onclick := onHistoryClick(hi) _,
      span(s"${hi.item.charinfos.length} pilots, "),
      span(hi.item.solarSystem.fold("")(ss => s"$ss ,")),
      hi.respTimeAgo
    )).render
  }

  def refreshResponseTimeAgo(): Unit = {
    localHistory.history.foreach { h =>
      h.refreshResponseTimeAgo()
    }
  }

  override def formSubmitAction(ev: dom.Event, tsStarted: Long): Unit = {
    val pilotNames = pilotBox.value.split( """\n""").map(_.trim)
    val illegalNames = pilotNames.filterNot(EveCharacterName.isValidCharacterName)
    if (illegalNames.nonEmpty) {
      log.info("illegal pilot names: " + illegalNames.mkString(", "))
    }

    val validNames = pilotNames.filter(EveCharacterName.isValidCharacterName).toVector

    log.debug("calling listCharacters with " + validNames.length + " pilots")
    val req = ListCharactersRequest(SharedConfig.client.clientSoftwareVersion, validNames)
    val future = Ajaxer[Api].listCharacters(req).call()
    future.onComplete {
      case Failure(ex) =>
        submitError(tsStarted, ex)
      case Success(resp) =>
        submitSuccess(tsStarted, resp)
    }
  }

  def shareUrl(ev: dom.Event): Unit = {
    js.Dynamic.global.prompt("Ctrl/Cmd-C:", LittleHelper.getLocationUrl)
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

  def onHistoryClick(hi: HistoryItem[ListCharactersResponse])(ev: dom.Event): Unit = {
    ev.stopPropagation()
    ev.preventDefault()

    val myA = ev.findParent("pure-menu-link").asInstanceOf[html.Anchor]
    makeHistoryLinkActive(myA)

    LocalDetailsView.update(hi.item)
  }

  override def route(args: Seq[String]): Unit = {
    args match {
      case Seq(cachedKey) =>
        val req = CachedCharactersRequest(SharedConfig.client.clientSoftwareVersion, cachedKey)
        Ajaxer[Api].cachedCharacters(req).call().onSuccess {
          case None =>
            log.info("route: cached result does not exist: " + cachedKey)
          case Some(resp) =>
            log.info("route: retrieved cached dscan result: " + resp.charinfos.length)
            LocalDetailsView.update(resp)
        }
    }
  }

  /** get the panels fragment - changed by route! */
  override def urlFragment: String = (Vector("#localTab") ++ LocalDetailsView.resultCacheKey).mkString("/")
}
