package me.rjfarmer.rlh.eve

import java.text.NumberFormat
import java.util.Locale

import scala.io.Source

final case class CategoryAndGroup(category: String, group: String)

final case class DscanItem(name: String, typ: String, groupCat: CategoryAndGroup, distAu: Option[Double])

object DscanParser {

  private[this] val groupByType: Map[String, CategoryAndGroup] = {
    Map() ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("names_and_types.txt"))
    .getLines()
    .map { line =>
      val arr = line.split('|')
      arr(0) -> CategoryAndGroup(arr(3), arr(1))
    }
  }

  /**
   * Convert a string of the form to the distance in AU.
   *
   * - for no distance
   * 6,666 km for distance in km, divide by 149.598.000 for AU
   * 13.4 AU for distance in AU
   *
   * @param dist distance string
   * @return Some(distance in AU) or none
   */
  def parseDistance(dist: String): Option[Double] = {
    dist.split(' ') match {
      case Array(num, typ) =>
        val factor = typ match {
          case "km" => 1.0d / 149598000.0d
          case "AU" => 1.0d
          case _ => throw new IllegalArgumentException("can not parse distance type: " + dist)
        }
        val nf = NumberFormat.getInstance(Locale.US)
        Some(nf.parse(num).doubleValue * factor)
      case Array("-") =>
        None
      case _ =>
        throw new IllegalArgumentException("can not parse distance: " + dist)
    }
  }

  def parse(dscan: String): Seq[DscanItem] = {
    dscan.split( """\n""")
      .filterNot(s => s.isEmpty)
      .map { line =>
        val strings: Seq[String] = line.split( """\t""").map(_.trim)
        strings match {
          case Seq(name, typ, distStr) =>
            DscanItem(name, typ, groupByType(typ), parseDistance(distStr))
          case _ =>
            throw new IllegalArgumentException("can not parse line: " + line)
        }
      }
  }

  def main(args: Array[String]): Unit = {
    parse(Source.fromFile(args(0)).mkString).foreach { x =>
      println(x)
    }
  }

}
