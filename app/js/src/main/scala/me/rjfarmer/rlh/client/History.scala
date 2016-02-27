package me.rjfarmer.rlh.client

import me.rjfarmer.rlh.api.HasTimestampAndOptionalCacheKey

/** history items have text with time information about the response */
class HistoryItem[T <: HasTimestampAndOptionalCacheKey] (val item: T ) extends HasResponseTimeAgo {

  updateResponseTimestamp(item.timestamp)

}

/**
 * Maintains result history for T
 *
 * Results are sorted by timestamps (DESC).
 *
 * @tparam T result type
 */
class History [T <: HasTimestampAndOptionalCacheKey] (_vector: Vector[HistoryItem[T]]){

  private[this] val maxEntries = 16
  private[this] var items: Vector[HistoryItem[T]] = _vector

  def this() = {
    this(Vector())
  }

  def add(item: T): HistoryItem[T] = {
    val hItem = new HistoryItem(item)
    val already = items.find(x => x.item.cacheKey == item.cacheKey)
    already match {
      case None =>
        items = (items :+ hItem)
          .sorted(ByTimestamp)
          .take(maxEntries)
        hItem
      case Some(hItem) =>
        hItem
    }
  }

  def history: Vector[HistoryItem[T]] = items


  object ByTimestamp extends Ordering[HasResponseTimeAgo] {
    // descending!
    override def compare(x: HasResponseTimeAgo, y: HasResponseTimeAgo): Int = y.timestamp.compareTo(x.timestamp)
  }
}
