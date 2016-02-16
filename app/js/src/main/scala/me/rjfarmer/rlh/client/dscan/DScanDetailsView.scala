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

          for (item <- typItems.sorted(DScanDistanceOrdering)) {
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
    seq.groupBy(groupFn).toSeq.sortWith(dscanGroupSorter)
  }

  // sort by distance
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

  // sort by the size of the group, or otherwise the string extracted by fn
  def dscanGroupSorter(x: (String, Seq[DScanLine]), y: (String, Seq[DScanLine])) = {
    if (x._2.length == y._2.length) {
      x._1 < y._1
    } else {
      // reverse order!
      x._2.length > y._2.length
    }
  }


}
