package me.rjfarmer.rlh.client.logging

import java.util.Date

import org.scalajs.dom


/**
 * Logging Wrapper for Shared/JS Code.
 *
 * There is no console in the in-game browser, so we need a logging system.
 * For shared code, we'll just forward to slf4j
 *
 */
class LoggerRLH(val name: String, val level: Int) {

  def logMessage(logLevel: Int, msg: String, args: Any*): Unit = {
    import scalatags.JsDom.all._
    if (logLevel <= level) {
      val logmsg = (Vector(msg) ++ (args map {
        _.toString
      })).mkString(" ")
      val millis = new Date().getTime - LoggerRLHConfig.startMillis
      val child = tr(td(millis.toString),
        td(LoggerRLH.levelName(level)),
        td(logmsg)
      ).render
      dom.document.getElementById("logMessages").appendChild(child)
    }
  }

  def critical(msg: String, args: Any*): Unit = logMessage(LoggerRLH.CRITICAL, msg, args: _*)

  def severe(msg: String, args: Any*): Unit = logMessage(LoggerRLH.SEVERE, msg, args: _*)

  def error(msg: String, args: Any*): Unit = logMessage(LoggerRLH.ERROR, msg, args: _*)

  def warn(msg: String, args: Any*): Unit = logMessage(LoggerRLH.WARN, msg, args: _*)

  def info(msg: String, args: Any*): Unit = logMessage(LoggerRLH.INFO, msg, args: _*)

  def debug(msg: String, args: Any*): Unit = logMessage(LoggerRLH.DEBUG, msg, args: _*)

  def trace(msg: String, args: Any*): Unit = logMessage(LoggerRLH.TRACE, msg, args: _*)

}

object LoggerRLH {

  val CRITICAL = 0
  val SEVERE = 1
  val ERROR = 2
  val WARN = 3
  val INFO = 4
  val DEBUG = 5
  val TRACE = 6

  def apply(name: String) = new LoggerRLH(name, LoggerRLHConfig.logLevelFor(name))

  def levelName(level: Int) = level match {
    case CRITICAL => "CRITICAL"
    case SEVERE => "SEVERE"
    case ERROR => "ERROR "
    case WARN => "WARN  "
    case INFO => "info  "
    case DEBUG => "debug "
    case TRACE => "trace "
    case _ => "unknown"
  }

}

object LoggerRLHConfig {

  var defaultLevel = LoggerRLH.INFO

  var config = Map[String,Int]()

  val startMillis = new Date().getTime

  def logLevelFor(name: String): Int = {
    config.getOrElse(name, defaultLevel)
  }

}


