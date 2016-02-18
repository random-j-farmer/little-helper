package me.rjfarmer.rlh.server

import me.rjfarmer.rlh.api.{DScanParseRequest, DScanParseResponse}
import me.rjfarmer.rlh.eve.DScanParser
import org.ehcache.Cache

import scala.concurrent.Future

/**
 * DScan Request Handler
 *
 * @param cache response cache
 */
class DScanRequestHandler(val cache: ResponseCache[DScanParseResponse])
  extends CachingRequestHandler[DScanParseRequest, DScanParseResponse, DScanParseResponse] {

  override def clientVersionError(req: DScanParseRequest): DScanParseResponse = {
    DScanParseResponse(Some(Server.clientVersionError), None, req.solarSystem, Vector())
  }

  override def handleUncached(req: DScanParseRequest): Future[DScanParseResponse] = {
    Future.successful(
      try {
        DScanParseResponse(None, None, req.solarSystem, req.lines.map(DScanParser.parse))
      } catch {
        case ex: Exception =>
          DScanParseResponse(Some("Error parsing request lines: " + ex),
            None, req.solarSystem, Vector())
      })
  }
}

object DScanRequestHandler {

  def apply(cache: ResponseCache[DScanParseResponse]): DScanRequestHandler = new DScanRequestHandler(cache)

}

// dscan response cache helper
final case class DScanResponseCache(cache: Cache[String, DScanParseResponse]) extends ResponseCache[DScanParseResponse]
