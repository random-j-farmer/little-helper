package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.api.{ListCharactersResponse, WebserviceResult, CharInfo}
import me.rjfarmer.rlh.shared.SharedConfig

import scalatags.JsDom.all._


class ListCharactersView {

  var respTs = System.currentTimeMillis()
  val respTimeAgo = span(responseTimeAgo).render
  val pilotCount = span().render
  val solarSystem = span().render

  val corpList = tbody().render
  val pilotList = tbody().render

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

  def freshnessKlass(nowMillis: Long, wsr: WebserviceResult): String = {
    val relativeAge = (nowMillis - wsr.receivedTimestamp) / SharedConfig.client.staleOlderThanMillis.toDouble
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

  def zkillboardUrl(p: CharInfo): String = {
    p.characterID.fold("#")(id => s"""https://zkillboard.com/character/$id/""")
  }

  def update(resp: ListCharactersResponse) = {

    val pilots = resp.charinfos

    pilotCount.innerHTML = s"${resp.charinfos.size} pilots, "

    resp.solarSystem match {
      case None =>
      case Some(ssn) => solarSystem.appendChild(span(ssn, ", ").render)
    }

    val cutoff = math.max(2.0d, pilots.size / 10.0d)
    val byCorp: Map[AllianceOrCorp, Seq[CharInfo]] = pilots.groupBy(AllianceOrCorp.apply)
    val topCorps: Seq[Seq[CharInfo]] = byCorp.values.toSeq
      .filter(group => group.size >= cutoff)
      .sortWith((a, b) => a.size > b.size)
    val other = pilots.size - topCorps.map(_.size).sum

    corpList.innerHTML = ""
    for (group <- topCorps; aoc = AllianceOrCorp(group.head)) {
      corpList.appendChild(tr(
        td(a(href := aoc.uri, target := "_blank", aoc.name)),
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
        td(a(href := zkillboardUrl(p), target := "_blank", p.name)),
        td(corp.name, " (", byCorp(corp).size, ")"),
        td(p.recentKills.getOrElse(0) + "/" + p.recentLosses.getOrElse(0)),
        td(span("%4.2f".format(p.characterAge.getOrElse(-1.0d))),
          span(`class` := frKlass, title := frKlass.replace('-', ' '), style := "font-size: 150%", raw("&#8226;")))
      ).render
      pilotList.appendChild(trow)
    }

  }

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
      ci.corporation.fold(new Unknown(ci.name).asInstanceOf[AllianceOrCorp])(name => new Corp(name))
    }(new Alliance(_))
  }

}
