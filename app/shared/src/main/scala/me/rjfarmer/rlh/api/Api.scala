package me.rjfarmer.rlh.api

import scala.concurrent.Future

final case class EmploymentHistory(corporationID: String, corporationName: String, startDate: String)

final case class CharacterInfo(characterID: Long, characterName: String,
                               race: String, bloodline: String, ancestry: String,
                               corporationID: Long, corporation: String, corporationDate: String,
                               allianceID: Option[Long], alliance: Option[String], allianceDate: Option[String],
                               securityStatus: Double, characterAge: Double, employmentHistory: Seq[EmploymentHistory])


final case class CharacterIDAndName(characterID: Long, characterName: String)


final case class ZkActivePvP (kills: Int, regions: Int, ships: Int, systems: Int)

final case class ZkInfo(allianceID: Long, corporationID: Long, factionID: Long, id: Long, killID: Long,
                        name: String)

final case class ZkMonthStats(year: Int, month: Int, shipsLost: Int, pointsLost: Double, iskLost: Double,
                              shipsDestroyed: Int, pointsDestroyed: Double, iskDestroyed: Double)

final case class ZkStats(info: ZkInfo, activepvp: ZkActivePvP, lastMonths: ZkMonthStats)


final case class CharInfo(info: CharacterInfo, zkStats: ZkStats)



trait Api {

  def listCharacters(names: Seq[String]): Future[Seq[CharInfo]]

}

