package me.rjfarmer.rlh.client.dscan

import me.rjfarmer.rlh.api.{DScanLine, DScanParseResponse}
import me.rjfarmer.rlh.client.Refreshable
import me.rjfarmer.rlh.shared.DScanLineCheck

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

    for ((cat, catItems) <- groupAndSort(resp.lines, _.groupCat.category)) {
      val catClass = cssClass("cat", cat)
      val groupClasses = "grp " + catClass
      dscanList.appendChild(tr(`class` := "cat",
        td(cat, " (", catItems.length, ")"),
        td(), td(), td(), td()
      ).render)

      for ((group, groupItems) <- groupAndSort(catItems, _.groupCat.group)) {
        val groupClass = cssClass("grp", group)
        val typClasses = (Vector("typ") :+ catClass :+ groupClass).mkString(" ")
        dscanList.appendChild(tr(`class` := groupClasses,
          td(),
          td(group, " (", groupItems.length, ")"),
          td(), td(), td()
        ).render)

        for ((typ, typItems) <- groupAndSort(groupItems, _.typ)) {
          val typClass = cssClass("typ", typ)
          val lineClasses = (Vector("line") :+ catClass :+ groupClass :+ typClass).mkString(" ")
          dscanList.appendChild(tr(`class` := typClasses,
            td(), td(),
            td(typ, " (", typItems.length, ")"),
            td(), td()
          ).render)

          for (item <- groupItems.sorted(DScanDistanceOrdering)) {
            dscanList.appendChild(tr(`class` := lineClasses,
              td(), td(), td(),
              td(item.name),
              td(DScanLineCheck.formatDistance(item.distAu))
            ).render)
          }
        }
      }
    }
  }

  def cssClass(prefix: String, typGroupCat: String): String = {
    prefix + "_" + typGroupCat.replace(" ", "_").trim.toLowerCase
  }

  def groupAndSort(seq: Seq[DScanLine], groupFn: DScanLine => String): Seq[(String, Seq[DScanLine])] = {
    // lt function is > because we want descending order
    seq.groupBy(groupFn).toSeq.sortWith((p1, p2) => p1._2.length > p2._2.length)
  }

  object DScanDistanceOrdering extends Ordering[DScanLine] {
    override def compare(x: DScanLine, y: DScanLine): Int = {
      (x.distAu, y.distAu) match {
        case (None, None) => 0
        case (Some(_), None) => -1
        case (None, Some(_)) => 1
        case (Some(xd), Some(yd)) => Math.signum(xd - yd).toInt
      }
    }
  }

}
