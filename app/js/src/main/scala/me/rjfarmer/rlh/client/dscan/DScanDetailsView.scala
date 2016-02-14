package me.rjfarmer.rlh.client.dscan

import me.rjfarmer.rlh.api.DScanParseResponse

import scalatags.JsDom.all._

object DScanDetailsView {

  var respTs = System.currentTimeMillis()
  val respTimeAgo = span(responseTimeAgo).render
  val dscanItemCount = span().render
  val solarSystem = span().render

  val dscanList = tbody().render

  // XXX extract me
  def responseTimeAgo: String = {
    val totalSeconds = (System.currentTimeMillis() - respTs) / 1000L
    val totalMinutes = totalSeconds / 60
    val totalHours = totalMinutes / 60

    if (totalHours > 0) {
      s"""${totalHours}h ${totalMinutes % 60}m ago"""
    } else {
      s"""${totalMinutes}m ago"""
    }
  }

  def refreshResponseTimeAgo = {
    respTimeAgo.innerHTML = responseTimeAgo
    responseTimeAgo
  }

  def update(resp: DScanParseResponse) = {

    val lines = resp.lines

    dscanItemCount.innerHTML = s"${lines.size} scanned objects, "
    respTs = System.currentTimeMillis()
    refreshResponseTimeAgo

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
