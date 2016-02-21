package me.rjfarmer.rlh.cache

import org.ehcache.Cache

/**
 * Scala wrapped API for EHC Cache
 *
 * Necessary because the key values for long need to be java.lang.Long
 * which is different from Long.
 *
 * Also, working with options is nicer
 *
 * @tparam K key type
 * @tparam V value type
 */
trait EhcCache[K, V] {

  /** get an option from the underlying ehc cache */
  def get(k: K): Option[V]

  /** store into the underlying cache */
  def put(k: K, v: V): Unit

}

/** ehc cache with long keys */
class EhcLongCache[V](cache: Cache[java.lang.Long, V])
  extends EhcCache[Long, V] {

  def get(k: Long): Option[V] = Option(cache.get(k))

  def put(k: Long, v: V): Unit = cache.put(k, v)

}

/** ehc cache with string keys */
class EhcStringCache[V](cache: Cache[java.lang.String, V])
  extends EhcCache[String, V] {

  def get(k: String): Option[V] = Option(cache.get(k))

  def put(k: String, v: V): Unit = cache.put(k, v)

}

