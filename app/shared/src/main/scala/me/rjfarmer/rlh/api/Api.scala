package me.rjfarmer.rlh.api

import me.rjfarmer.rlh.shared.SharedConfig

import scala.concurrent.Future


/** all web service requests provide these, but they are filled in on the server in the IGB case */
trait WebserviceRequest {

  def clientIP: String

  def pilot: Option[String]

  def solarSystem: Option[String]

}

sealed trait WebserviceResult {

  def receivedTimestamp: Long

  def isFresh: Boolean = {
    receivedTimestamp + SharedConfig.client.staleOlderThanMillis > System.currentTimeMillis()
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


final case class ZkActivePvP(kills: Int, regions: Int, ships: Int, systems: Int)

final case class ZkInfo(allianceID: Long, corporationID: Long, factionID: Long, id: Long, killID: Long,
                        name: String)

final case class ZkMonthStats(year: Int, month: Int, shipsLost: Int, pointsLost: Double, iskLost: Double,
                              shipsDestroyed: Int, pointsDestroyed: Double, iskDestroyed: Double)

final case class ZkStats(info: ZkInfo, activepvp: ZkActivePvP, lastMonths: ZkMonthStats, receivedTimestamp: Long)
  extends WebserviceResult


/**
 * Can be cached for sharing of results.
 *
 * @tparam T cached response type
 */
trait CachableResponse[+T] extends Serializable {

  def cacheKey: Option[String]

  def copyWithCacheKey(key: String): T

}

/** has version information that can be checked */
trait VersionedRequest extends WebserviceRequest {

  def version: String

}

/** retrieves a cached prior request */
trait CachedRequest extends VersionedRequest {

  def cacheKey: String

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
 * @param receivedTimestamp oldest timestamp of involved result objects
 * @param characterAge character age.  character info may not be there for large requests
 * @param corporation corporation name.  character info may not be there for large requests
 * @param alliance alliance name.  character info may not be there for large requests
 * @param recentKills kills in last 2 months. zkinfo probably not there for large requests
 * @param recentLosses losses in last 2 months.  zkinfo probably not there for large requests
 */
final case class CharInfo(name: String,
                          characterID: Option[Long],
                          complete: Boolean,
                          receivedTimestamp: Long,

                          characterAge: Option[Double],
                          corporation: Option[String],
                          alliance: Option[String],

                          recentKills: Option[Int],
                          recentLosses: Option[Int])
  extends WebserviceResult

object CharInfo {

  def apply(name: String, id: Option[Long], oci: Option[CharacterInfo], ozk: Option[ZkStats]): CharInfo = {
    val complete = oci.isDefined && ozk.isDefined
    val oldest = math.min(oci.fold(0L)(ci => ci.receivedTimestamp), ozk.fold(0L)(zk => zk.receivedTimestamp))
    new CharInfo(oci.fold(name)(_.characterName),
      id, complete = complete, oldest,
      oci.map(_.characterAge), oci.map(_.corporation), oci.flatMap(_.alliance),
      ozk.map(_.lastMonths.shipsDestroyed), ozk.map(_.lastMonths.shipsLost))
  }
}

final case class ListCharactersRequest(version: String, names: Vector[String],
                                       // these three are not currently filled in by the client
                                       // but by the server when reading request headers
                                       clientIP: String, pilot: Option[String], solarSystem: Option[String])
  extends VersionedRequest

final case class CachedCharactersRequest(version: String, cacheKey: String,
                                         // filled in on the server
                                         clientIP: String, pilot: Option[String], solarSystem: Option[String])
  extends CachedRequest

final case class ListCharactersResponse(message: Option[String],
                                        cacheKey: Option[String],
                                        // may be present if IGB
                                        solarSystem: Option[String],
                                        charinfos: Vector[CharInfo])
  extends CachableResponse[ListCharactersResponse] {

  override def copyWithCacheKey(key: String): ListCharactersResponse = copy(cacheKey = Some(key))

}


//
// DSCAN API
//

final case class CategoryAndGroup(category: String, group: String)

final case class DScanLine(name: String, typ: String, groupCat: CategoryAndGroup, distAu: Option[Double])

final case class DScanParseRequest(version: String, lines: Vector[String],
                                   // again, filled in by server from request headers
                                   clientIP: String, pilot: Option[String], solarSystem: Option[String])
  extends VersionedRequest

final case class DScanParseResponse(message: Option[String],
                                    cacheKey: Option[String],
                                    // my be present if IGB
                                    solarSystem: Option[String],
                                    lines: Vector[DScanLine])
  extends CachableResponse[DScanParseResponse] {

  override def copyWithCacheKey(key: String): DScanParseResponse = copy(cacheKey = Some(key))

}

final case class CachedDScanRequest(version: String, cacheKey: String,
                                    clientIP: String, pilot: Option[String], solarSystem: Option[String])
  extends CachedRequest


trait Api {

  def listCharacters(request: ListCharactersRequest): Future[ListCharactersResponse]

  def cachedCharacters(request: CachedCharactersRequest): Future[Option[ListCharactersResponse]]

  def parseDScan(request: DScanParseRequest): Future[DScanParseResponse]

  def cachedDScan(request: CachedDScanRequest): Future[Option[DScanParseResponse]]

}



