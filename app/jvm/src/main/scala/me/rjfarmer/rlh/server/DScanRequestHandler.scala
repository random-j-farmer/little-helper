package me.rjfarmer.rlh.server

import me.rjfarmer.rlh.api.{DScanParseRequest, DScanParseResponse}
import me.rjfarmer.rlh.cache.{EhcStringCache, EhcCache}
import me.rjfarmer.rlh.eve.DScanParser
import me.rjfarmer.rlh.server.Boot._
import org.ehcache.Cache

import scala.concurrent.Future

/**
 * DScan Request Handler
 *
 * @param cache response cache
 */
class DScanRequestHandler(val cache: EhcCache[String, DScanParseResponse])
  extends CachingRequestHandler[DScanParseRequest, DScanParseResponse] {

  override def clientVersionError(req: DScanParseRequest): DScanParseResponse = {
    DScanParseResponse(Some(Server.clientVersionError), None, req.solarSystem, System.currentTimeMillis(), Vector())
  }

  override def handleUncached(req: DScanParseRequest): Future[DScanParseResponse] = {
    Future.successful(
      try {
        bootSystem.log.info("<{}> parseDScan: successful response for {} objects",
          req.clientIP, req.lines.size)
        DScanParseResponse(None, None, req.solarSystem, System.currentTimeMillis(), req.lines.map(DScanParser.parse))
      } catch {
        case ex: Exception =>
          DScanParseResponse(Some("Error parsing request lines: " + ex),
            None, req.solarSystem, System.currentTimeMillis(), Vector())
      })
  }

  /** abstract method that will produce a response with cachekey added */
  override def copyWithCacheKey(key: String, resp: DScanParseResponse): DScanParseResponse = resp.copy(cacheKey = Some(key))

}

object DScanRequestHandler {

  def apply(cache: EhcCache[String, DScanParseResponse]): DScanRequestHandler = new DScanRequestHandler(cache)

}

// dscan response cache helper
class DScanResponseCache(cache: Cache[String, DScanParseResponse]) extends EhcStringCache[DScanParseResponse](cache)
