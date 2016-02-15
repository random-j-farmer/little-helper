package me.rjfarmer.rlh.client

import scalatags.JsDom.all._

/**
 * It needs the refresh!
 *
 */
trait Refreshable {

  val respTimeAgo = span(responseTimeAgo).render
  private[this] var respTs = System.currentTimeMillis()
  refreshResponseTimeAgo

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

  def updateResponseTimestamp(): Unit = {
    respTs = System.currentTimeMillis()
  }

}
