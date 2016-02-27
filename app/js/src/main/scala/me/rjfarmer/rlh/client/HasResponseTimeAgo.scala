package me.rjfarmer.rlh.client

import scalatags.JsDom.all._

/**
 * It has a responseTimeAgo span that needs updating
 *
 */
trait HasResponseTimeAgo {

  val respTimeAgo = span(responseTimeAgo).render
  private[this] var respTs = System.currentTimeMillis()
  refreshResponseTimeAgo()

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

  def refreshResponseTimeAgo(): Unit = {
    respTimeAgo.innerHTML = responseTimeAgo
  }

  def timestamp: Long = respTs

  def updateResponseTimestamp(ts: Long): Unit = {
    respTs = ts
  }

}
