package me.rjfarmer.rlh.server

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.security.MessageDigest

import me.rjfarmer.rlh.api.{HasCacheKeyAndVersion, HasVersion}
import me.rjfarmer.rlh.cache.EhcCache
import me.rjfarmer.rlh.server.Boot._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}


/** handles client-server version mismatch */
trait VersionRequestHandler[Q <: HasVersion, T] {

  /** error response (usually not exception!) for client-version mismatch */
  def clientVersionError(req: Q): T

  /** handle a request with correct version */
  def handleVersion(req: Q): Future[T]

  def handleRequest(req: Q): Future[T] = {
    if (req.version != BuildInfo.version) {
      Promise.successful(clientVersionError(req)).future
    } else {
      handleVersion(req)
    }
  }

}

/** a caching request handler */
trait CachingRequestHandler[Q <: HasVersion, T <: Serializable]
  extends VersionRequestHandler[Q, T] {

  /** the responsecache that will be used for caching */
  def cache: EhcCache[String, T]

  /** abstract method that will produce the uncached response */
  def handleUncached(req: Q): Future[T]

  /** abstract method that will produce a response with cachekey added */
  def copyWithCacheKey(key: String, resp: T): T

  def handleVersion(req: Q): Future[T] = {
    val future = handleUncached(req)
    future.map { resp =>
      val key = cacheKey(resp)
      val value = copyWithCacheKey(key, resp)
      cache.put(cacheKey(resp), value)
      bootSystem.log.info("<{}> caching result {}",
        req.clientIP, key)
      value
    }
  }

  private def cacheKey(resp: T): String = {
    val md = MessageDigest.getInstance("SHA1")
    md.digest(serialize(resp)).map("%02x".format(_)).mkString
  }

  private def serialize(resp: T): Array[Byte] = {
    val bout = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bout)
    oos.writeObject(resp)
    oos.close()
    bout.toByteArray
  }

  // Q is needed for the copyWithKey operation, not for retrieval
  def cachedResponse(req: HasCacheKeyAndVersion): Future[Option[T]] = {
    bootSystem.log.info("<{}> cached response {}",
      req.clientIP, req.cacheKey)
    Future.successful(cache.get(req.cacheKey))
  }

}
