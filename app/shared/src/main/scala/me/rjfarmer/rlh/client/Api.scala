package me.rjfarmer.rlh.client

case class PilotInfo(id: String, name: String, allianceOrCorp: String)

trait Api {

  def listPilots(pilots: Array[String]): Seq[PilotInfo]

}
