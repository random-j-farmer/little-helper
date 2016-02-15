package me.rjfarmer.rlh.client.dscan

import me.rjfarmer.rlh.api.DScanParseResponse
import me.rjfarmer.rlh.client.Refreshable

import scalatags.JsDom.all._

object DScanDetailsView extends Refreshable {

  val dscanItemCount = span().render
  val solarSystem = span().render

  val dscanList = tbody().render

  def update(resp: DScanParseResponse) = {
    updateResponseTimestamp()
    refreshResponseTimeAgo

    val lines = resp.lines

    dscanItemCount.innerHTML = s"${lines.size} scanned objects, "

    resp.solarSystem match {
      case None =>
      case Some(ssn) => solarSystem.appendChild(span(ssn, ", ").render)
    }

    dscanList.innerHTML = ""
    for (item <- lines) {
      val trow = tr(
        td(item.name),
        td(item.typ),
        td(item.groupCat.group),
        td(item.groupCat.category),
        td(item.distAu)
      ).render
      dscanList.appendChild(trow)
    }

  }

}
