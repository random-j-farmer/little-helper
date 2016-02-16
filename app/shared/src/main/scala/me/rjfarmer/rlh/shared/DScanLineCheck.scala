package me.rjfarmer.rlh.shared

object DScanLineCheck {

  private[this] val kmToAU = 149598000.0d

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
        val divBy = typ match {
          case "km" => kmToAU
          case "AU" => 1.0d
          case _ => throw new IllegalArgumentException("can not parse distance type: " + dist)
        }
        Some(num.replace(",", "").toDouble / divBy)
      case Array("-") =>
        None
      case _ =>
        throw new IllegalArgumentException("can not parse distance: " + dist)
    }
  }

  /**
   * Parse a dscan line.
   *
   * @param line line from the dscan
   * @return
   */
  def parseDScanLine(line: String): (String, String, Option[Double]) = {
    val strings: Seq[String] = line.split( """\t""").map(_.trim)
    strings match {
      case Seq(name, typ, distStr) =>
        if (typ.isEmpty) {
          throw new IllegalArgumentException("no type in dscan line: " + line)
        }
        (name, typ, parseDistance(distStr))
      case _ =>
        throw new IllegalArgumentException("can not parse line: " + line)
    }
  }

  def isValidDScanLine(line: String): Boolean = {
    try {
      parseDScanLine(line)
      true
    } catch {
      case _: IllegalArgumentException =>
        false
    }
  }


  def formatDistance(distAu: Option[Double]): String = {
      distAu match {
        case None =>
          "-"
        case Some(d) =>
          if (d < 0.01d) f"${d*kmToAU}%2.0f km" else f"$d%2.2f AU"
      }
  }

}
