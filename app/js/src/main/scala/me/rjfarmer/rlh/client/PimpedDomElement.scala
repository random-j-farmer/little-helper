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

  private def classSet: Set[String] = {
    val klass: Option[String] = Option(elem.getAttribute("class"))
    val empty = Set[String]()
    klass.fold(empty)(s => empty ++ s.split(" "))
  }

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

  /** find a parent element with a given class */
  def findParent(klass: String): dom.Element = {

    def rec(pimp: PimpedDomElement): dom.Element = {
      val parent = Option(pimp.elem.parentNode.asInstanceOf[dom.Element])
      if (pimp.hasClass(klass)) {
        pimp.elem.asInstanceOf[dom.Element]
      } else {
        rec(parent.get)
      }
    }

    rec(this)
  }

  /** insert thew new child as the first element */
  def insertChild(child: dom.Element): Unit = {
    if (elem.childElementCount == 0) {
      elem.appendChild(child)
    } else {
      val firstChild = elem.children.item(0)
      elem.insertBefore(child, firstChild)
    }
  }

}


object PimpedDomElement {

  implicit def apply(elem: dom.Element): PimpedDomElement = {
    if (elem == null) {
      throw new IllegalArgumentException("PimpedDomElement.apply: elem.null")
    }
    new PimpedDomElement(elem)
  }

  implicit def pimpEventTarget(ev: dom.Event): PimpedDomElement = {
    new PimpedDomElement(ev.target.asInstanceOf[dom.Element])
  }

  /** get the documents cookies split into a map */
  def cookieMap: Map[String, String] = {
    val empty = Map.empty[String, String]
    Option(dom.document.cookie) match {
      case None =>
        empty
      case Some(allCookies) =>
        empty ++ allCookies.split(';')
          .flatMap { s: String =>
            s.split("=", 2) match {
              case Array(k, v) =>
                Array(k -> v)
              case _ =>
                Array[Pair[String, String]]()
            }
          }.toSeq
    }
  }

}
