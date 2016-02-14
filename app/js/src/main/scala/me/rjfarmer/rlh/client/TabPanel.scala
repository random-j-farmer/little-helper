package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.client.logging.LoggerRLH
import org.scalajs.dom
import org.scalajs.dom.html
import org.scalajs.dom.raw.{HTMLAnchorElement, HTMLLIElement, HTMLDivElement}

import scalatags.JsDom.all._


object TabPanel {

  def apply(tabs: Seq[TabbedPanel]): TabPanel = new TabPanel(tabs)

}

/**
 * A TabPanel contains multiple TabbedPanel
 *
 * It provides a menu to switch between its tabs.
 *
 * @param tabs the tabs
 */
class TabPanel(val tabs: Seq[TabbedPanel]) {

  private val log = LoggerRLH("client.TabPanel")

  private val tabById: Map[String, TabbedPanel] = Map() ++ tabs.map(tab => tab.panelView.getAttribute("id") -> tab)

  private val linkListItems: Seq[HTMLLIElement] = tabs.map { tab =>
    li(a(href := "#" + tab.panelView.getAttribute("id"), cls := "pure-menu-link", tab.panelName)).render
  }

  activatePanel(tabs.head.panelView.getAttribute("id"))

  private def activatePanel(panelID: String) = {
    val frag = "#" + panelID

    linkListItems.foreach { item =>
      val href = item.childNodes(0).asInstanceOf[HTMLAnchorElement].getAttribute("href")
      item.setAttribute("class", if (href == frag) "pure-menu-item pure-menu-selected" else "pure-menu-item")
    }
    tabs.foreach { tab =>
      tab.panelView.setAttribute("hidden", "hidden")
    }
    tabById(panelID).panelView.removeAttribute("hidden")
  }


  val linksView: HTMLDivElement =
    div(cls := "pure-menu pure-menu-horizontal", onclick := menuClick _,
      style := "display: inline; position: absolute; top: 0px; right: 0px; width: 10cm; text-align: right;",
      ul(cls := "pure-menu-list", linkListItems)
    ).render

  val panelView: HTMLDivElement = div(tabs.map(_.panelView)).render

  def menuClick(ev: dom.Event): Unit = {
    try {
      ev.stopPropagation()
      val myA = ev.target.asInstanceOf[html.Anchor]
      val myLI = myA.parentNode.asInstanceOf[html.LI]
      val myUL = myLI.parentNode.asInstanceOf[html.UList]
      val linkTarget = myA.href.replaceFirst( """.*\#""", "")
      // log.info("linkTarget: " + linkTarget)

      activatePanel(linkTarget)
    } catch {
      case ex: Exception => log.error("Exception:", ex)
    }
  }

}
