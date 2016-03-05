package me.rjfarmer.rlh.server

import scala.io.Source

object PrivateConfig {

  def readPrivateConfig: Option[PrivateConfig] = {
    try {
      val src = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("private_config.json"), "utf-8")
      try {
        Some(upickle.default.read[PrivateConfig](src.mkString))
      } finally {
        src.close()
      }
    } catch {
      case ex: Exception =>
        System.err.println("readPrivateConfig: error: " + ex)
        None
    }
  }

}

final case class PrivateConfig(jwtSecret: String, crestConfig: CrestConfig)

final case class CrestConfig(clientID: String,
                             redirectUrl: String,
                             clientSecret: String)
