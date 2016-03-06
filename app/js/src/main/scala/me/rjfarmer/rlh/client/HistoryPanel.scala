package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.api.CacheableResponse
import me.rjfarmer.rlh.client.PimpedDomElement._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLLIElement

import scalatags.JsDom.all._

/**
 * A panel with some kind of history.
 *
 */
trait HistoryPanel[T <: CacheableResponse] {

  /** The history object that remembers the responses */
  def history: History[T]

  /** The history view */
  val historyView = ul(cls := "pure-menu-list").render

  /** make a view item from a response history item */
  def historyItemView(hi: HistoryItem[T]): HTMLLIElement

  /** update the details view with an item selected from the history */
  def updateDetailsView(item: T): Unit

  def addResultToHistory(resp: T, empty: Boolean): Unit = {
    if (!empty) {
      val hView = historyItemView(history.add(resp))
      historyView.insertChild(hView)
      handleCurrentHistoryLink(Some(hView))
    } else {
      handleCurrentHistoryLink(None)
    }
  }

  def handleCurrentHistoryLink(linkOpt: Option[HTMLLIElement]): Unit = {
    val nodeList = historyView.getElementsByTagName("LI")
    for (i <- 0 until nodeList.length) {
      nodeList.item(i).asInstanceOf[html.Anchor].removeClass("pure-menu-selected")
    }
    if (linkOpt.isDefined) {
      linkOpt.get.addClass("pure-menu-selected")
    }
  }

  def onHistoryClick(hi: HistoryItem[T])(ev: dom.Event): Unit = {
    ev.stopPropagation()
    ev.preventDefault()

    val listElem = ev.findParent("pure-menu-item").asInstanceOf[HTMLLIElement]

    handleCurrentHistoryLink(Some(listElem))

    updateDetailsView(hi.item)
  }

  def refreshResponseTimeAgo(): Unit = {
    history.history.foreach { h =>
      h.refreshResponseTimeAgo()
    }
  }

}
