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
 * @tparam K key type
 */
class RetrieveQueue[K] {

  type RType = Retrievable[K]
  type QType = Queue[Retrievable[K]]
  type VQType = Vector[QType]

  private[this] val holder: AtomicReference[VQType] = new AtomicReference(Vector(Queue(), Queue(), Queue()))

  def enqueue(prio: Int)(item: RType): Unit = updateAndGet { queues =>
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

object RetrieveQueue {

  def apply[T](): RetrieveQueue[T] = new RetrieveQueue[T]()

}
