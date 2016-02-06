package me.rjfarmer.rlh.client

import org.scalajs.dom

class PimpedDomElement(val elem: dom.Element) {

  def addClass(klass: String): Unit = {
    val classes = (Set[String]() ++ elem.getAttribute("class").split(" ")) + klass
    elem.setAttribute("class", classes.mkString(" "))
  }

  def removeClass(klass: String): Unit = {
    val classes = (Set[String]() ++ elem.getAttribute("class").split(" ")) - klass
    elem.setAttribute("class", classes.mkString(" "))
  }

}


object PimpedDomElement {

  implicit def apply(elem: dom.Element) = new PimpedDomElement(elem)

}
