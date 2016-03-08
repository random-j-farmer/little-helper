package me.rjfarmer.rlh.api

import me.rjfarmer.rlh.shared.SharedConfig

import scala.concurrent.Future


sealed trait HasTimestamp {

  def timestamp: Long

  def isFresh: Boolean = {
    timestamp + SharedConfig.client.staleOlderThanMillis > System.currentTimeMillis()
  }

}

final case class EmploymentHistory(corporationID: String, corporationName: String, startDate: String)

final case class CharacterInfo(characterID: Long, characterName: String,
                               race: String, bloodline: String, ancestry: String,
                               corporationID: Long, corporation: String, corporationDate: String,
                               allianceID: Option[Long], alliance: Option[String], allianceDate: Option[String],
                               securityStatus: Double, characterAge: Double, employmentHistory: Seq[EmploymentHistory],
                               timestamp: Long)
  extends HasTimestamp


final case class CharacterIDAndName(characterID: Long, characterName: String, timestamp: Long)
  extends HasTimestamp


final case class ZkActivePvP(kills: Int, regions: Int, ships: Int, systems: Int)

final case class ZkInfo(allianceID: Long, corporationID: Long, factionID: Long, id: Long, killID: Long,
                        name: String)

final case class ZkMonthStats(year: Int, month: Int, shipsLost: Int, pointsLost: Double, iskLost: Double,
                              shipsDestroyed: Int, pointsDestroyed: Double, iskDestroyed: Double)

final case class ZkStats(info: ZkInfo, activepvp: ZkActivePvP, lastMonths: ZkMonthStats, timestamp: Long)
  extends HasTimestamp


/** has version information that can be checked */
trait HasVersion {

  def version: String

}

/** has cache key information */
trait HasCacheKey {

  def cacheKey: String

}

// for easy use in type parameters
trait HasCacheKeyAndVersion extends HasVersion with HasCacheKey
trait CacheableResponse extends HasTimestamp {
  def cacheKey: Option[String]
}


/**
 * CharInfo contains info from eve xml api and zkillboard rest requests.
 *
 * In case of timeouts (hello, zkillboard) we may use a cached response
 * for longer than intended, or if we do not even have a stale response
 * the info will not be there.

 * @param name character name.  its in the input, will always be there.
 * @param characterID character ID. will usually be present
 * @param complete complete response with all info present (although it may be stale)
 * @param timestamp oldest timestamp of involved result objects
 * @param characterAge character age.  character info may not be there for large requests
 * @param corporation corporation name.  character info may not be there for large requests
 * @param alliance alliance name.  character info may not be there for large requests
 * @param recentKills kills in last 2 months. zkinfo probably not there for large requests
 * @param recentLosses losses in last 2 months.  zkinfo probably not there for large requests
 */
final case class CharInfo(name: String,
                          characterID: Option[Long],
                          complete: Boolean,
                          timestamp: Long,

                          characterAge: Option[Double],
                          corporation: Option[String],
                          alliance: Option[String],

                          recentKills: Option[Int],
                          recentLosses: Option[Int])
  extends HasTimestamp

object CharInfo {

  def apply(name: String, id: Option[Long], oci: Option[CharacterInfo], ozk: Option[ZkStats]): CharInfo = {
    val complete = oci.isDefined && ozk.isDefined
    val oldest = math.min(oci.fold(0L)(ci => ci.timestamp), ozk.fold(0L)(zk => zk.timestamp))
    new CharInfo(oci.fold(name)(_.characterName),
      id, complete = complete, oldest,
      oci.map(_.characterAge), oci.map(_.corporation), oci.flatMap(_.alliance),
      ozk.map(_.lastMonths.shipsDestroyed), ozk.map(_.lastMonths.shipsLost))
  }
}

final case class ListCharactersRequest(version: String, names: Vector[String])
  extends HasVersion

final case class CachedCharactersRequest(version: String, cacheKey: String)
  extends HasCacheKeyAndVersion

final case class ListCharactersResponse(message: Option[String],
                                        cacheKey: Option[String],
                                        // may be present if IGB
                                        solarSystem: Option[String],
                                        timestamp: Long,
                                        charinfos: Vector[CharInfo])
  extends CacheableResponse


//
// DSCAN API
//

final case class CategoryAndGroup(category: String, group: String)

final case class DScanLine(name: String, typ: String, groupCat: CategoryAndGroup, distAu: Option[Double])

final case class DScanParseRequest(version: String, lines: Vector[String])
  extends HasVersion

final case class DScanParseResponse(message: Option[String],
                                    cacheKey: Option[String],
                                    // my be present if IGB
                                    solarSystem: Option[String],
                                    timestamp: Long,
                                    lines: Vector[DScanLine])
  extends CacheableResponse

final case class CachedDScanRequest(version: String, cacheKey: String)
  extends HasCacheKeyAndVersion


trait Api {

  def listCharacters(request: ListCharactersRequest): Future[ListCharactersResponse]

  def cachedCharacters(request: CachedCharactersRequest): Future[Option[ListCharactersResponse]]

  def parseDScan(request: DScanParseRequest): Future[DScanParseResponse]

  def cachedDScan(request: CachedDScanRequest): Future[Option[DScanParseResponse]]

}



