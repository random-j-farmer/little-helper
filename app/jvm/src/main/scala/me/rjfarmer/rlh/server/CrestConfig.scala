package me.rjfarmer.rlh.server

import scala.io.Source
import scala.util.Try

object CrestConfig {

  def readCrestConfig: Try[CrestConfig] = {
    Try {
      val src = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("crest_config.json"), "utf-8")
      try {
        upickle.default.read[CrestConfig](src.mkString)
      } finally {
        src.close()
      }
    }
  }

}

final case class CrestConfig(clientID: String,
                             redirectUrl: String,
                             clientSecret: String)
