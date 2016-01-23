package me.rjfarmer.rlh.client

// for correct macro application

import autowire._
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.logging.LoggerRLH
import me.rjfarmer.rlh.shared.EveCharacterName
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
// if the rest api is slow, alliance or corp might not be known
final case class Unknown(name: String) extends AllianceOrCorp {
  val typ: String = "pilot"
}

object AllianceOrCorp {

  def apply(ci: CharInfo): AllianceOrCorp = {
    ci.alliance.fold {
      ci.corporation.fold (new Unknown(ci.name).asInstanceOf[AllianceOrCorp])( name => new Corp(name))
    } (new Alliance(_))
  }

}


@JSExport
object LittleHelper {

  val log = LoggerRLH("client.LittleHelper")

  val version = dom.document.getElementById("version").textContent
  log.info("Client version: " + version)

  var respTs = System.currentTimeMillis()
  val respTimeAgo = span().render
  val pilotCount = span().render
  val solarSystem = span().render

  val pilotBox = textarea(cols:=20, rows:=10).render
  pilotBox.onfocus = (ev: dom.Event) => pilotBox.value = ""

  val messageBox = div(hidden, `class`:="info").render
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

  def submitStarted: Long = {
    addClass(submitButton, "pure-button-disabled")
    System.currentTimeMillis()
  }

  def submitSuccess(started: Long, resp: ListCharactersResponse): Unit = {
    val now = System.currentTimeMillis
    val pilots = resp.charinfos

    log.info("listCharacters: received " + pilots.size + " pilots in " +
      resp.solarSystem + " in " +
      (now - started) + "ms")

    removeClass(submitButton, "pure-button-disabled")

    handleMessageBox(resp.message)

    respTs = System.currentTimeMillis()
    respTimeAgo.innerHTML = responseTimeAgo
    pilotCount.innerHTML = s"${pilots.size} pilots, "
    solarSystem.innerHTML = ""
    resp.solarSystem match {
      case None =>
      case Some(ssn) => solarSystem.appendChild(span(ssn, ", ").render)
    }

    val cutoff = math.max(2.0d, pilots.size/10.0d)
    val byCorp: Map[AllianceOrCorp, Seq[CharInfo]] = pilots.groupBy(AllianceOrCorp.apply)
    val topCorps: Seq[Seq[CharInfo]] = byCorp.values.toSeq
      .filter(group => group.size >= cutoff)
      .sortWith((a,b) => a.size > b.size)
    val other = pilots.size - topCorps.map(_.size).sum

    corpList.innerHTML = ""
    for (group <- topCorps; aoc = AllianceOrCorp(group.head)) {
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
    val nowMillis = System.currentTimeMillis()
    for (p <- pilots; frKlass = freshnessKlass(nowMillis, p); corp = AllianceOrCorp(p)) {
      val trow = tr(
        td( a(href:=zkillboardUrl(p), target:="_blank", p.name)),
        td(corp.name, " (", byCorp(corp).size, ")"),
        td(p.recentKills.getOrElse(0) + "/" + p.recentLosses.getOrElse(0)),
        td(span("%4.2f".format(p.characterAge.getOrElse(-1.0d))),
          span(`class`:=frKlass, title:=frKlass.replace('-', ' '), style:="font-size: 150%", raw("&#8226;")))
      ).render
      pilotList.appendChild(trow)
    }
  }

  def handleMessageBox(msg: Option[String]): Unit = {
    msg match {
      case None =>
        messageBox.setAttribute("hidden", "hidden")
        messageBox.innerHTML = ""
      case Some(txt) =>
        messageBox.innerHTML = ""
        messageBox.appendChild(span(txt).render)
        messageBox.removeAttribute("hidden")
    }
  }

  def freshnessKlass(nowMillis: Long, wsr: WebserviceResult): String = {
    val relativeAge = (nowMillis - wsr.receivedTimestamp) / Api.apiRequestTimeoutMills.toDouble
    if (relativeAge < 0.5d) {
      "fresh"
    } else if (relativeAge < 1.0d) {
      "getting-stale"
    } else if (relativeAge < 2.0d) {
      "stale"
    } else {
      "out-of-date"
    }
  }

  def submitError(started: Long, ex: Throwable): Unit = {
    val now = System.currentTimeMillis
    log.error("Ajax Exception after " + (now - started) + "ms: " + ex)
    removeClass(submitButton, "pure-button-disabled")

    handleMessageBox(Some("Error communicating with the server"))
  }

  def formSubmit(ev: dom.Event): Unit = {
    ev.preventDefault()

    val tsStarted = submitStarted

    val pilotNames = pilotBox.value.split("""\n""").map(_.trim)
    val illegalNames = pilotNames.filterNot(EveCharacterName.isValidCharacterName)
    if (illegalNames.nonEmpty) {
      log.info("illegal pilot names: " + illegalNames.mkString(", "))
    }

    val validNames = pilotNames.filter(EveCharacterName.isValidCharacterName).toVector

    log.debug("calling listCharacters with " + validNames.length + " pilots")
    val req = ListCharactersRequest(version, validNames, None, None)
    val future = Ajaxer[Api].listCharacters(req).call()
    future.onFailure { case ex: Throwable =>
      submitError(tsStarted, ex)
    }
    future.foreach { response => submitSuccess(tsStarted, response) }
  }

  def zkillboardUrl(p: CharInfo): String = {
    p.characterID.fold("#")(id => s"""https://zkillboard.com/character/$id/""")
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
            messageBox,
            h1(pilotCount, solarSystem, respTimeAgo),
            h2("Pilots by Alliance/Corp"),
            table(cls:="pure-table pure-table-striped",
              thead(tr(th("Alliance/Corp"), th("# Pilots"))),
              corpList),
            h2("Pilots"),
            table(cls:="pure-table pure-table-striped",
              thead(tr(th("Name"), th("Alliance/Corp"), th("Kills/Deaths"), th("Age"))),
              pilotList))
      ).render
    )

    dom.window.setInterval(refreshResponseTimeAgo _, 10000d)
  }

}
