package me.rjfarmer.rlh.client.dscan

import me.rjfarmer.rlh.api.{DScanLine, DScanParseResponse}
import me.rjfarmer.rlh.client.{LittleHelper, HasResponseTimeAgo}
import me.rjfarmer.rlh.shared.DScanLineCheck
import org.scalajs.dom
import org.scalajs.dom.html

import scala.scalajs.js
import scalatags.JsDom.all._

object DScanDetailsView extends HasResponseTimeAgo {

  import me.rjfarmer.rlh.client.PimpedDomElement._

  // private[this] val log = LoggerRLH("client.dscan.DScanTab")

  val dscanItemCount = span("0 objects, ").render
  val solarSystem = span().render

  private val noCelestialMessage = "No celestial on D-Scan."
  val nearestCelestial = span(noCelestialMessage).render

  val dscanList = tbody().render

  /** current result cache key, if any */
  var resultCacheKey: Option[String] = None
  val resultUrlBox = input(cls := "pure-input-2-3", `type` := "text", readonly).render
  resultUrlBox.onfocus = selectAllOnFocus _

  // priorities for category sorting.  higher value => bigger priority.
  private[this] val categoryPriorities: Map[String, Int] = Map(
    "Ship" -> 10,
    "Deployable" -> 9,
    "Celestial" -> -10
  )
  private[this] val noPriorities: Map[String, Int] = Map()

  def selectAllOnFocus(ev: dom.Event) = {
    js.timers.setTimeout(50.0d)(resultUrlBox.select())
  }

  def update(resp: DScanParseResponse) = {

    resultCacheKey = resp.cacheKey
    LittleHelper.setLocationFragment(s"#dscanTab/${resp.cacheKey.get}")
    resultUrlBox.value = LittleHelper.getLocationUrl

    updateResponseTimestamp(resp.timestamp)
    refreshResponseTimeAgo

    val lines = resp.lines

    dscanItemCount.innerHTML = s"${lines.size} objects, "

    resp.solarSystem match {
      case None =>
      case Some(ssn) => solarSystem.appendChild(span(ssn, ", ").render)
    }

    nearestCelestial.innerHTML = ""
    nearestCelestial.appendChild(span(findNearestCelestial(resp)).render)

    dscanList.innerHTML = ""

    for ((cat, catItems) <- groupAndSort(resp.lines, categoryPriorities, _.groupCat.category)) {
      val catClass = cssClass("cat", cat)
      val groupParents = Set() + catClass
      dscanList.appendChild(treeRow(catClass, "", Set(), cat + " (" + catItems.length + ")"))

      for ((group, groupItems) <- groupAndSort(catItems, noPriorities, _.groupCat.group)) {
        val groupClass = cssClass("grp", group)
        val typParents = groupParents + groupClass
        dscanList.appendChild(treeRow(groupClass, catClass, groupParents, group + " (" + groupItems.length + ")"))

        for ((typ, typItems) <- groupAndSort(groupItems, noPriorities, _.typ)) {
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

  private val celestialCategories = Set("Celestial")

  private def findNearestCelestial(resp: DScanParseResponse): String = {
    resp.lines
      .filter(line => celestialCategories.contains(line.groupCat.category))
      .sorted(DScanDistanceOrdering)
      .headOption
      .map(cel => cel.name + " at " + DScanLineCheck.formatDistance(cel.distAu))
      .getOrElse(noCelestialMessage)
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
      .replace( """[()]""".r.regex, "")
      .trim.toLowerCase
  }

  def groupAndSort(seq: Seq[DScanLine], priorityMap: Map[String, Int], groupFn: DScanLine => String): Seq[(String, Seq[DScanLine])] = {
    // we just use the implicit ordering for tuples, but this means we can't have the seq[scaline] in there
    val grouped = seq.groupBy(groupFn)
    grouped.toSeq
      .map(pair => (-priorityMap.getOrElse(pair._1, 0), -pair._2.length, pair._1))
      .sorted
      .map(tup => tup._3 -> grouped(tup._3))
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

}
