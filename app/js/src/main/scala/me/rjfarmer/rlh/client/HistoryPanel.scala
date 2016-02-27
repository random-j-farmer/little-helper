package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.api.{ListCharactersResponse, HasTimestampAndOptionalCacheKey}
import me.rjfarmer.rlh.client.PimpedDomElement._
import me.rjfarmer.rlh.client.local.LocalDetailsView
import me.rjfarmer.rlh.client.local.LocalTab._
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLLIElement

import scalatags.JsDom.all._

/**
 * A panel with some kind of history.
 *
 */
trait HistoryPanel[T <: HasTimestampAndOptionalCacheKey] {

  /** The history object that remembers the responses */
  def history: History[T]

  /** The history view */
  val historyView = ul(cls := "pure-menu-list").render

  /** make a view item from a response history item */
  def historyItemView(hi: HistoryItem[T]): HTMLLIElement

  /** update the details view with an item selected from the history */
  def updateDetailsView(item: T): Unit

  def addResultToHistory(resp: T): Unit = {
    val hView = historyItemView(history.add(resp))
    historyView.insertChild(hView)
    val hViewLink = hView.getElementsByTagName("A").item(0).asInstanceOf[html.Anchor]
    makeHistoryLinkActive(hViewLink)
  }

  def makeHistoryLinkActive(link: html.Anchor): Unit = {
    val nodeList = historyView.getElementsByTagName("A")
    for (i <- 0 until nodeList.length) {
      nodeList.item(i).asInstanceOf[html.Anchor].removeClass("pure-menu-selected")
    }
    link.addClass("pure-menu-selected")
  }

  def onHistoryClick(hi: HistoryItem[T])(ev: dom.Event): Unit = {
    ev.stopPropagation()
    ev.preventDefault()

    val myA = ev.findParent("pure-menu-link").asInstanceOf[html.Anchor]
    makeHistoryLinkActive(myA)

    updateDetailsView(hi.item)
  }

  def refreshResponseTimeAgo(): Unit = {
    history.history.foreach { h =>
      h.refreshResponseTimeAgo()
    }
  }





}
