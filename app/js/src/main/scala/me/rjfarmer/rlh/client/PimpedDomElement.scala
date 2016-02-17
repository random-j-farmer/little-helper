package me.rjfarmer.rlh.client

import org.scalajs.dom

/**
 * Pimped DOM Element
 *
 * adds addClass/removeClass
 *
 * @param elem pimp the element
 */
class PimpedDomElement(val elem: dom.Element) {

  @inline private def classSet: Set[String] = Set[String]() ++ elem.getAttribute("class").split(" ")

  /** add klass to the elements list of classes */
  def addClass(klass: String): Unit = {
    val classes = classSet + klass
    elem.setAttribute("class", classes.mkString(" "))
  }

  /** remove klass from the elements list of classes */
  def removeClass(klass: String): Unit = {
    val classes = classSet - klass
    elem.setAttribute("class", classes.mkString(" "))
  }

  /** does the element have klass? */
  def hasClass(klass: String): Boolean = {
    classSet.contains(klass)
  }

  /** toggle the class and return the new status */
  def toggleClass(klass: String): Boolean = {
    val old = hasClass(klass)
    if (old) removeClass(klass) else addClass(klass)
    ! old
  }

  /** the first class of the element from the list of klasses */
  def findClass(klasses: Seq[String]): Option[String] = {
    klasses.find(classSet)
  }

}


object PimpedDomElement {

  implicit def apply(elem: dom.Element): PimpedDomElement = new PimpedDomElement(elem)

}
