package me.rjfarmer.rlh.eve

import jawn.ast.{JObject, JValue}

import scala.annotation.tailrec

object PimpedJValue {

  implicit def apply(jv: JValue): PimpedJValue = new PimpedJValue(jv)

}

/**
 * Improved JValue object.
 *
 * @param jv jvalue
 */
class PimpedJValue(jv: JValue) {

  /** recursive get */
  def recget(path: String*) = {

    @tailrec
    def rec(v: JValue, p: List[String]): JValue = {
      p match {
        case Nil =>
          v
        case x :: xs =>
          rec(v.get(x), xs)
      }
    }

    rec(jv, path.toList)
  }

  /** get an int */
  def getInt: Option[Int] = jv.getLong.map(_.toInt)

  /** get at the object properties */
  def asMap: collection.mutable.Map[String, JValue] = {
    jv.asInstanceOf[JObject].vs
  }

  /** asMap, but returns empty map for null values */
  def asMapOrEmpty: collection.mutable.Map[String, JValue] = {
    if (jv.isNull) {
      collection.mutable.Map()
    } else {
      asMap
    }
  }

}
