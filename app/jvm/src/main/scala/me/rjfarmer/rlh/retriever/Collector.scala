package me.rjfarmer.rlh.retriever

import akka.actor._
import me.rjfarmer.rlh.api.HasTimestamp
import me.rjfarmer.rlh.cache.EhcCache
import scala.concurrent.duration._

import scala.util.{Failure, Success, Try}


object Collector {

  def props[K, V <: HasTimestamp](cache: EhcCache[K,V], cached: Map[K,V], numItems: Int, replyTo: ActorRef, timeout: FiniteDuration): Props = {
    Props(new Collector(cache, cached, numItems, replyTo, timeout))
  }

  case object Timeout

  case class Result[K, V](item: Retrievable[K],
                          result: Try[V]) {
    def pair: (K,V) = (item.key, result.get)
  }

}

/** Collect a number of results or return incomplete result after timeout */
class Collector[K,V <: HasTimestamp] (cache: EhcCache[K,V], cached: Map[K,V], numResults: Int, replyTo: ActorRef, timeout: FiniteDuration)
  extends Actor with ActorLogging {

  type TResult = Collector.Result[K,V]

  var received: Map[K,V] = Map()
  var numErrors = 0
  var replied = false

  override def preStart(): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global

    super.preStart()

    context.system.scheduler.scheduleOnce(timeout, self, Collector.Timeout)
  }


  override def receive: Receive = {

    case result: TResult =>
      if (! replied) {
        // log.debug("received: {}", result)
        result.result match {
          case Success(v) =>
            received += result.pair
          case Failure(ex) =>
            numErrors += 1
        }
        if (received.size + numErrors == numResults) {
          finish(false)
        }
      }

    case Collector.Timeout =>
      // XXX actor is only destroyed after timeout expires ...
      finish(true)

    case msg =>
      log.warning("unknown message: {}", msg)

  }

  def finish(poison: Boolean): Unit = {
    if (! replied) {
      val result = cached ++ received
      replyTo ! result
      replied = true
    }
    if (poison) {
      self ! PoisonPill
    }
  }


}
