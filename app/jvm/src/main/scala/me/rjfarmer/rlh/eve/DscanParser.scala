package me.rjfarmer.rlh.eve

import me.rjfarmer.rlh.api.{CategoryAndGroup, DScanLine}
import me.rjfarmer.rlh.shared.DScanLineCheck

import scala.io.Source

object DScanParser {

  private[this] val groupByType: Map[String, CategoryAndGroup] = {
    Map() ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("names_and_types.txt"))
    .getLines()
    .map { line =>
      val arr = line.split('|')
      arr(0) -> CategoryAndGroup(arr(3), arr(1))
    }
  }

  def parse(line: String): DScanLine = {
    val (name, typ, dist) = DScanLineCheck.parseDScanLine(line)
    DScanLine(name, typ, groupByType(typ), dist)
  }

}
