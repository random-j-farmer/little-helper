package me.rjfarmer.rlh.client.dscan

import me.rjfarmer.rlh.api.{DScanLine, DScanParseResponse}
import me.rjfarmer.rlh.client.Refreshable
import me.rjfarmer.rlh.client.logging.LoggerRLH
import me.rjfarmer.rlh.shared.DScanLineCheck
import org.scalajs.dom
import org.scalajs.dom.html

import scalatags.JsDom.all._

object DScanDetailsView extends Refreshable {

  import me.rjfarmer.rlh.client.PimpedDomElement._

  private[this] val log = LoggerRLH("client.dscan.DScanTab")

  val dscanItemCount = span().render
  val solarSystem = span().render

  val dscanList = tbody().render

  def update(resp: DScanParseResponse) = {
    updateResponseTimestamp(resp.timestamp)
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
      val groupParents = Set() + catClass
      dscanList.appendChild(treeRow(catClass, "", Set(), cat + " (" + catItems.length + ")"))

      for ((group, groupItems) <- groupAndSort(catItems, _.groupCat.group)) {
        val groupClass = cssClass("grp", group)
        val typParents = groupParents + groupClass
        dscanList.appendChild(treeRow(groupClass, catClass, groupParents, group + " (" + groupItems.length + ")"))

        for ((typ, typItems) <- groupAndSort(groupItems, _.typ)) {
          val typClass = cssClass("typ", typ)
          val leafParents = typParents + typClass
          dscanList.appendChild(treeRow(typClass, groupClass, typParents, typ + " (" + typItems.length + ")"))

          for (item <- typItems.sorted(DScanDistanceOrdering)) {
            dscanList.appendChild(leafRow(typClass, leafParents, item.name, DScanLineCheck.formatDistance(item.distAu)))
          }
        }
      }
    }
  }

  /**
   * Create a new tree row.
   *
   * The row is open by default.
   *
   * @param nodeClass the class of this row
   * @param ctrlClass the class that controls this row, if any
   * @param parentClasses all classes above this element
   * @param text the text
   * @return tree node
   */
  def treeRow(nodeClass: String, ctrlClass: String, parentClasses: Set[String], text: String) = {
    tr(`class` := (parentClasses + "open").mkString(" "),
      onclick := nonLeafClick _,
      data.klass := nodeClass, data.ctrl := ctrlClass,
      td(span(`class` := "level" + (parentClasses.size + 1), text)),
      td()
    ).render
  }

  /**
   * Create a new leaf tree row.
   *
   * @param ctrlClass class that controls this row
   * @param parentClasses all classes above this element
   * @param text the text
   * @param dist distance
   * @return leaf tree node
   */
  def leafRow(ctrlClass: String, parentClasses: Set[String], text: String, dist: String) = {
    tr(`class` := (parentClasses + "leaf").mkString(" "),
      data.ctrl := ctrlClass,
      td(span(`class` := "level" + (parentClasses.size + 1), text)),
      td(dist)
    ).render

  }

  def nonLeafClick(ev: dom.Event): Unit = {
    ev.stopPropagation()
    var elem = ev.target.asInstanceOf[html.Element]
    while (elem != null && !elem.isInstanceOf[html.TableRow]) {
      elem = elem.parentElement
    }
    if (elem != null) {
      val nodeClass = elem.getAttribute("data-klass")
      val isOpen = elem.toggleClass("open")
      val rows = dscanList.rows

      if (isOpen) {
        for (i <- 0 until rows.length; row = rows(i)) {
          // open only the nodes that are controlled immediately!
          val ctrlClass = row.getAttribute("data-ctrl")
          if (ctrlClass == nodeClass) {
            row.removeAttribute("hidden")
          }
        }
      } else {
        for (i <- 0 until rows.length; row = rows(i)) {
          // close all the nodes below this one!
          if (row.hasClass(nodeClass)) {
            row.setAttribute("hidden", "hidden")
            // leafs don't have open/closed!
            row.removeClass("open")
          }
        }
      }
    }
  }

  def cssClass(prefix: String, typGroupCat: String): String = {
    prefix + "_" + typGroupCat.replace(" ", "_")
      .replace("""[()]""".r.regex, "")
      .trim.toLowerCase
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
