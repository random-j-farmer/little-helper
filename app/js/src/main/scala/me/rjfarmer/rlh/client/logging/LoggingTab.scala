package me.rjfarmer.rlh.client.logging

import me.rjfarmer.rlh.client.TabbedPanel
import org.scalajs.dom

import scalatags.JsDom.all._


object LoggingTab extends TabbedPanel {

  override val panelName = "Logging"

  override val panelView = div(id := "loggingTab", hidden,
    h2("Log Messages"),
    button(id := "clearLogButton", cls := "pure-button pure-button-primary",
      `type` := "button", "Clear Log", onclick := clearLogButtonClick _),
    br(),
    table(cls := "pure-table pure-table-striped", width := "100%",
      col(width := "10%"), col(width := "10%"), col(width := "80%"),
      thead(
        th("ms"), th("Level"), th("Message")),
      tbody(id := "logMessages")
    )
  ).render

  def clearLogButtonClick(ev: dom.Event): Unit = dom.document.getElementById("logMessages").innerHTML = ""

  /** get the panels fragment - changed by route! */
  override def urlFragment: String = "#loggingTab"
}
