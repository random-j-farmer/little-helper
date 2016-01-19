package me.rjfarmer.rlh.api

import scala.concurrent.Future

trait WebserviceResult {

  def receivedTimestamp: Long

  def isFresh: Boolean = {
    receivedTimestamp + Api.apiRequestTimeoutMills > System.currentTimeMillis()
  }
}

final case class EmploymentHistory(corporationID: String, corporationName: String, startDate: String)

final case class CharacterInfo(characterID: Long, characterName: String,
                               race: String, bloodline: String, ancestry: String,
                               corporationID: Long, corporation: String, corporationDate: String,
                               allianceID: Option[Long], alliance: Option[String], allianceDate: Option[String],
                               securityStatus: Double, characterAge: Double, employmentHistory: Seq[EmploymentHistory],
                               receivedTimestamp: Long)
  extends WebserviceResult


final case class CharacterIDAndName(characterID: Long, characterName: String, receivedTimestamp: Long)
  extends WebserviceResult


final case class ZkActivePvP (kills: Int, regions: Int, ships: Int, systems: Int)

final case class ZkInfo(allianceID: Long, corporationID: Long, factionID: Long, id: Long, killID: Long,
                        name: String)

final case class ZkMonthStats(year: Int, month: Int, shipsLost: Int, pointsLost: Double, iskLost: Double,
                              shipsDestroyed: Int, pointsDestroyed: Double, iskDestroyed: Double)

final case class ZkStats(info: ZkInfo, activepvp: ZkActivePvP, lastMonths: ZkMonthStats, receivedTimestamp: Long)
  extends WebserviceResult


/**
 * CharInfo contains info from eve xml api and zkillboard rest requests.
 *
 * In case of timeouts (hello, zkillboard) we may use a cached response
 * for longer than intended, or if we do not even have a stale response
 * the info will not be there.

 * @param name character name.  its in the input, will always be there.
 * @param characterID character ID. will usually be present
 * @param complete complete response with all info present (although it may be stale)
 * @param fresh response will all info in defined caching time ranges
 * @param characterAge character age.  character info may not be there for large requests
 * @param corporation corporation name.  character info may not be there for large requests
 * @param alliance alliance name.  character info may not be there for large requests
 * @param recentKills kills in last 2 months. zkinfo probably not there for large requests
 * @param recentLosses losses in last 2 months.  zkinfo probably not there for large requests
 */
final case class CharInfo(name: String,
                         characterID: Option[Long],
                         complete: Boolean,
                         fresh: Boolean,

                         characterAge: Option[Double],
                         corporation: Option[String],
                         alliance: Option[String],

                         recentKills: Option[Int],
                         recentLosses: Option[Int])

object CharInfo {

  def apply(name: String, oci: Option[CharacterInfo], ozk: Option[ZkStats]): CharInfo = {
    val complete = oci.isDefined && ozk.isDefined
    new CharInfo(oci.fold(name)(_.characterName),
      oci.map(_.characterID), complete = complete,
      oci.exists(_.isFresh) && ozk.exists(_.isFresh),
      oci.map(_.characterAge), oci.map(_.corporation), oci.flatMap(_.alliance),
      ozk.map(_.lastMonths.shipsDestroyed), ozk.map(_.lastMonths.shipsLost))
  }
}

trait Api {

  def listCharacters(names: Vector[String]): Future[Vector[CharInfo]]

}


object Api {

  val apiRequestTimeoutMills = 6L * 3600L * 1000L

}




