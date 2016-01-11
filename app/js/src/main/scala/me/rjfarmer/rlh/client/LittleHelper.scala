package me.rjfarmer.rlh.client

// for correct macro application

import autowire._
import me.rjfarmer.rlh.api.{Api, CharInfo}
import me.rjfarmer.rlh.logging.LoggerRLH
import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html

import scala.scalajs.concurrent.JSExecutionContext.Implicits.runNow
import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scalatags.JsDom.all._

object Ajaxer extends autowire.Client[String, upickle.default.Reader, upickle.default.Writer] {
  override def doCall(req: Request) = {
    Ajax.post(url = "/ajax/" + req.path.mkString("/"), data=upickle.default.write(req.args)).map(_.responseText)
  }

  def read[Result: upickle.default.Reader](p:String) = upickle.default.read[Result](p)
  def write[Result: upickle.default.Writer](r: Result) = upickle.default.write(r)
}

trait AllianceOrCorp {
  def name: String

  def typ: String

  def uri: String = {
    s"http://evewho.com/$typ/$name"
  }
}

final case class Alliance(name: String) extends AllianceOrCorp {
  val typ: String = "alli"
}
final case class Corp(name: String) extends AllianceOrCorp {
  val typ: String = "corp"
}


@JSExport
object LittleHelper {

  val log = LoggerRLH("client.LittleHelper")


  var respTs = System.currentTimeMillis()
  val respTimeAgo = span().render

  val pilotBox = textarea(cols:=20, rows:=10).render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  val corpList = tbody().render
  val pilotList = tbody().render

  val submitButton = button(cls:="pure-button pure-button-primary", `type`:="submit", "Submit").render

  def clearLogButtonClick(ev: dom.Event): Unit = {
    dom.document.getElementById("logMessages").innerHTML = ""
  }

  def stringify(obj: js.Any): String = js.Dynamic.global.JSON.stringify(obj).asInstanceOf[String]

  def menuClick(ev: dom.Event): Unit = {
    try {
      ev.stopPropagation()
      val myA = ev.target.asInstanceOf[html.Anchor]
      val myLI = myA.parentNode.asInstanceOf[html.LI]
      val myUL = myLI.parentNode.asInstanceOf[html.UList]
      val linkTarget =  myA.href.replaceFirst(""".*\#""", "")
      // log.info("linkTarget: " + linkTarget)

      val children = myUL.children
      for (i <- 0 until children.length) {
        // log.info("removing pure-menu-selected:", children.item(i))
        children.item(i).setAttribute("class", "pure-menu-item")
      }
      // log.info("adding pure-menu-selected", myLI)
      myLI.setAttribute("class", "pure-menu-item pure-menu-selected")

      for (theId <- Seq("rlhMain", "rlhLogging")) {
        dom.document.getElementById(theId).setAttribute("hidden", "hidden")
      }
      dom.document.getElementById(linkTarget).removeAttribute("hidden")
    } catch {
      case ex: Exception => log.error("Exception:", ex)
    }
  }

  def addClass(elem: dom.Element, klass: String): Unit = {
    val classes = (Set[String]() ++ elem.getAttribute("class").split(" ")) + klass
    elem.setAttribute("class", classes.mkString(" "))
  }

  def removeClass(elem: dom.Element, klass: String): Unit = {
    val classes = (Set[String]() ++ elem.getAttribute("class").split(" ")) - klass
    elem.setAttribute("class", classes.mkString(" "))
  }

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

  def allianceOrCorp(p: CharInfo): AllianceOrCorp = {
    p.info.alliance match {
      case None => Corp(p.info.corporation)
      case Some(alli) => Alliance(alli)
    }
  }

  def submitStarted: Long = {
    addClass(submitButton, "pure-button-disabled")
    System.currentTimeMillis()
  }

  def submitSuccess(started: Long, pilots: Seq[CharInfo]): Unit = {
    val now = System.currentTimeMillis
    log.info("listCharacters: received " + pilots.size + " pilots in " + (now - started) + "ms")

    removeClass(submitButton, "pure-button-disabled")

    respTs = System.currentTimeMillis()
    respTimeAgo.innerHTML = responseTimeAgo

    val cutoff = math.max(2.0d, pilots.size/10.0d)
    val byCorp: Seq[Seq[CharInfo]] = pilots.groupBy(allianceOrCorp).values.toSeq
      .filter(group => group.size >= cutoff)
      .sortWith((a,b) => a.size > b.size)
    val other = pilots.size - byCorp.map(_.size).sum

    corpList.innerHTML = ""
    for (group <- byCorp; aoc = allianceOrCorp(group.head)) {
      corpList.appendChild(tr(
        td(a(href:=aoc.uri, target:="_blank", aoc.name)),
        td(group.size)).render)
    }
    if (other > 0) {
      corpList.appendChild(tr(
        td("Other"),
        td(other)).render)
    }

    pilotList.innerHTML = ""
    for (p <- pilots) {
      val trow = tr(id:=p.info.characterID,
        td( a(href:=zkillboardUrl(p), target:="_blank", p.info.characterName)),
        td(allianceOrCorp(p).name),
        td(p.zkStats.lastMonths.shipsDestroyed + "/" + p.zkStats.lastMonths.shipsLost),
        td(p.zkStats.activepvp.kills),
        td("%4.2f".format(p.info.characterAge))
      ).render
      pilotList.appendChild(trow)
    }
  }

  def submitError(started: Long, ex: Throwable): Unit = {
    val now = System.currentTimeMillis
    log.error("Ajax Exception after " + (now - started) + "ms: " + ex)
    removeClass(submitButton, "pure-button-disabled")
  }

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    val pilotNames = pilotBox.value.split("""\n""")
    log.debug("calling listCharacters with " + pilotNames.size + " pilots")
    val future = Ajaxer[Api].listCharacters(pilotNames).call()
    future.onFailure { case ex: Throwable =>
      submitError(tsStarted, ex)
    }
    future.foreach { pilots => submitSuccess(tsStarted, pilots) }
  }

  def zkillboardUrl(p: CharInfo): String = {
    s"""https://zkillboard.com/character/${p.info.characterID}/"""
  }

  @JSExport
  def main(container: html.Div) = {
    log.info("LittleHelper.main called")
    dom.document.getElementById("clearLogButton").asInstanceOf[html.Button].onclick = clearLogButtonClick _
    dom.document.getElementById("rlhMenu").asInstanceOf[html.Span].onclick = menuClick _

    container.appendChild(
        div(cls:="pure-g",
          form(cls:="pure-u-1-3 pure-form pure-form-stacked",
            onsubmit := formSubmit _,
            pilotBox,
            submitButton),
          div(cls:="pure-u-2-3",
            h1(respTimeAgo),
            h2("Pilot count by Alliance/Corp"),
            table(cls:="pure-table pure-table-striped",
              thead(tr(th("Alliance/Corp"), th("# Pilots"))),
              corpList),
            h2("Pilots"),
            table(cls:="pure-table pure-table-striped",
              thead(tr(th("Name"), th("Alliance/Corp"), th("Kills/Deaths"), th("Active"), th("Age"))),
              pilotList))
      ).render
    )

    dom.window.setInterval(refreshResponseTimeAgo _, 10000d)
  }

}
