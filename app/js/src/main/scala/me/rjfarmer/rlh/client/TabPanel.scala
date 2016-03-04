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

  private val tabById: Map[String, TabbedPanel] = Map() ++ tabs.map(tab => tab.panelID -> tab)

  private val linkListItems: Seq[HTMLLIElement] = tabs.map { tab =>
    li(a(href := "#" + tab.panelID, cls := "pure-menu-link", tab.panelName)).render
  }

  activatePanel(tabs.head.panelID)

  def activatePanel(panelID: String, changeBrowserLocation: Boolean = false) = {
    val frag = "#" + panelID

    linkListItems.foreach { item =>
      val href = item.childNodes(0).asInstanceOf[HTMLAnchorElement].getAttribute("href")
      item.setAttribute("class", if (href == frag) "pure-menu-item pure-menu-selected" else "pure-menu-item")
    }
    tabs.foreach { tab =>
      tab.panelView.setAttribute("hidden", "hidden")
    }
    val active = tabById(panelID)
    active.panelView.removeAttribute("hidden")

    if (changeBrowserLocation) {
      log.debug("activate panel: setting browser location: " + active.urlFragment)
      LittleHelper.setLocationFragment(active.urlFragment)
    }

    active
  }


  /** the view with the links that switch between tabs */
  val linksView: HTMLDivElement =
    div(cls := "pure-menu pure-menu-horizontal", onclick := menuClick _,
      ul(cls := "pure-menu-list", linkListItems)
    ).render

  /** the view with the panels that are hidden/shown as needed */
  val panelView: HTMLDivElement = div(tabs.map(_.panelView)).render


  private def menuClick(ev: dom.Event): Unit = {
    try {
      ev.stopPropagation()
      ev.preventDefault()
      val myA = ev.target.asInstanceOf[html.Anchor]
      val myLI = myA.parentNode.asInstanceOf[html.LI]
      val myUL = myLI.parentNode.asInstanceOf[html.UList]
      val linkTarget = myA.href.replaceFirst( """.*\#""", "")
      // log.info("linkTarget: " + linkTarget)

      activatePanel(linkTarget, changeBrowserLocation = true)
    } catch {
      case ex: Exception => log.error("Exception:", ex)
    }
  }

  /**
   * Route to the panel described by fragmentPath.
   *
   * fragmentPath is the panel id followed by optional arguments
   *
   * @param fragmentPath sequence of strings
   */
  def route(fragmentPath: Seq[String]): Unit = {
    log.info("routing to: " + fragmentPath)
    val active = activatePanel(fragmentPath.head)
    val tail = fragmentPath.tail
    if (tail.nonEmpty) {
      active.route(tail)
    }
  }

}
