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

  /** route the panel. default doesn't do anything */
  def route(args: Seq[String]): Unit = {}

}
