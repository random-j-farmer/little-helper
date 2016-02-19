package me.rjfarmer.rlh.server

import java.io.{ByteArrayOutputStream, ObjectOutputStream}
import java.security.MessageDigest

import me.rjfarmer.rlh.api.{CachedRequest, CachableResponse, VersionedRequest}
import me.rjfarmer.rlh.server.Boot._
import org.ehcache.Cache

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}


/** handles client-server version mismatch */
trait VersionRequestHandler[Q <: VersionedRequest, T] {

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

/** response cache interface */
trait ResponseCache[T] {

  def cache: Cache[java.lang.String, T]

  def get(key: String): Option[T] = Option(cache.get(key))

  def put(key: String, value: T): Unit = cache.put(key, value)

}

/** a caching request handler */
trait CachingRequestHandler[Q <: VersionedRequest, T, C <: CachableResponse[T]]
  extends VersionRequestHandler[Q, C] {

  def cache: ResponseCache[C]

  def handleUncached(req: Q): Future[C]

  def handleVersion(req: Q): Future[C] = {
    val future = handleUncached(req)
    future.map { resp =>
      val key = cacheKey(resp)
      val value = resp.copyWithCacheKey(key).asInstanceOf[C]
      cache.put(cacheKey(resp), value)
      bootSystem.log.info("<{}> caching result {}",
        req.clientIP, key)
      value
    }
  }

  private def cacheKey(resp: C): String = {
    val md = MessageDigest.getInstance("SHA1")
    md.digest(serialize(resp)).map("%02x".format(_)).mkString
  }

  private def serialize(resp: C): Array[Byte] = {
    val bout = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bout)
    oos.writeObject(resp)
    oos.close()
    bout.toByteArray
  }

  // Q is needed for the copyWithKey operation, not for retrieval
  def cachedResponse(req: CachedRequest): Future[Option[C]] = {
    bootSystem.log.info("<{}> cached response {}",
      req.clientIP, req.cacheKey)
    Future.successful(cache.get(req.cacheKey))
  }

}
