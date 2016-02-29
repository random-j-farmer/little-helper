package me.rjfarmer.rlh.client

import org.scalajs.dom.raw.HTMLDivElement


/**
 * A tabbed panel, which is inside the TabPanel
 *
 */
trait TabbedPanel {

  /** panel name, shown in the button */
  def panelName: String

  /** view to be shown/hidden. top div needs id */
  def panelView: HTMLDivElement

  /** route the panel */
  def route(args: Seq[String]): Unit = {}

  /** panel id without trailing params */
  def panelID: String

  /** get the panels fragment - changed by route! */
  def urlFragment: String = "#" + panelID

}
