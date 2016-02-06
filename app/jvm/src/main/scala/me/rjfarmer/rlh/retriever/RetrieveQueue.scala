package me.rjfarmer.rlh.retriever

import java.util.concurrent.atomic.AtomicReference

import scala.annotation.tailrec
import scala.collection.immutable.Queue

/**
 * A queue for a background retriever.
 *
 * A background retriever only caches the objects it retrieves,
 * it does not actually send them anywhere.  So entries
 * for the same item can be merged into just one entry.
 *
 * The priorities are reversed: 0 is highest priority, queues.length - 1 lowest
 *
 * There are two more priorities than configured via appliction.conf/PriorityConfig.
 * The first one is for uncached requests that are larger than the largest priority-by-size.
 * The second one is the catch-all priority for very large stale-refresh requests.
 *
 * @tparam K key type
 */
class RetrieveQueue[K] (numPriorities: Int) {

  type RType = Retrievable[K]
  type QType = Queue[Retrievable[K]]
  type VQType = Vector[QType]

  private[this] val holder: AtomicReference[VQType] = new AtomicReference(Vector.fill(numPriorities + 2)(Queue()))

  def enqueue(item: RType): Unit = updateAndGet { queues =>
    val prio = Math.min(item.priority, queues.length - 1)
    queues.updated(prio, queues(prio).enqueue(item))
  }


  def dequeueOption: Option[RType] = {
    var rc: Option[RType] = None

    updateAndGet { queues =>
      @tailrec def loop(i: Int): VQType = {
        if (i < queues.length) {
          val opt = queues(i).dequeueOption
          opt match {
            case None =>
              loop(i + 1)
            case Some((item, dq)) =>
              rc = Some(item)
              queues.updated(i, dq)
          }
        } else {
          queues
        }
      }
      loop(0)
    }
    rc
  }

  @tailrec
  private[this]
  def updateAndGet(func: (VQType) => VQType): VQType = {
    val oldQueues = holder.get()
    val newQueues = func(oldQueues)
    if (holder.compareAndSet(oldQueues, newQueues)) {
      newQueues
    } else {
      updateAndGet(func)
    }
  }

}

final case class PriorityConfig(prioritiesBySize: Vector[Int],
                                promoteStales: Vector[Int],
                                stalePriorityOffset: Int) {

  def priority(numUncached: Int): Int = {
    @tailrec def iter(i: Int): Int = {
      if (i < prioritiesBySize.length) {
        if (numUncached <= prioritiesBySize(i)) i else iter(i + 1)
      } else {
        i
      }
    }
    iter(0)
  }

  def promote(numUncached: Int): Int = {
    val highPrio = priority(numUncached)
    val numPromote = promoteStales(Math.min(promoteStales.length - 1, priority(numUncached)))
    if (highPrio < prioritiesBySize.length) {
      val highPrioLimit = prioritiesBySize(highPrio)
      numPromote + highPrioLimit - numUncached
    } else {
      // no limit for this priority (i.e. it's the one after the last configured limit)
      numPromote
    }
  }

}


object RetrieveQueue {

  def apply[T](numPriorities: Int): RetrieveQueue[T] = new RetrieveQueue[T](numPriorities)

  def apply[T](prioConf: PriorityConfig): RetrieveQueue[T] = apply[T](prioConf.prioritiesBySize.length + 1)

}
