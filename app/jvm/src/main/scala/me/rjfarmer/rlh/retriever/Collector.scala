package me.rjfarmer.rlh.retriever

import akka.actor._
import scala.concurrent.duration._

import scala.util.{Failure, Success, Try}


object Collector {

  def props[K, V](cache: RetrieveCache[K,V], cached: Map[K,V], numItems: Int, replyTo: ActorRef, timeout: FiniteDuration): Props = {
    Props(new Collector(cache, cached, numItems, replyTo, timeout))
  }

  case object Timeout

  case class Result[K, V](item: Retrievable[K],
                          result: Try[V]) {
    def pair: (K,Try[V]) = (item.key, result)
  }

}

/** Collect a number of results or return incomplete result after timeout */
class Collector[K,V] (cache: RetrieveCache[K,V], cached: Map[K,V], numResults: Int, replyTo: ActorRef, timeout: FiniteDuration)
  extends Actor with ActorLogging {

  type TResult = Collector.Result[K,V]

  var received: Map[K,V] = Map()
  var numErrors = 0
  var replied = false

  override def preStart(): Unit = {
    super.preStart()
    context.system.scheduler.scheduleOnce(timeout, self, Collector.Timeout)
  }


  override def receive: Receive = {

    case result: TResult =>
      if (! replied) {
        log.debug("received: {}", result)
        result.result match {
          case Success(v) =>
            received += result.pair
          case Failure(ex) =>
            numErrors += 1
        }
        if (received.size + numErrors == numResults) {
          finish()
        }
      }

    case Collector.Timeout =>
      log.debug("timeout expired")
      finish()

    case msg =>
      log.warning("unknown message: {}", msg)

  }

  def finish(): Unit = {
    // reget from cache - we are only sent the uncached elements
    // stale cached ones may have been refreshed
    val fromCache = cached.map(pair => (pair._1, cache.get(pair._1)))
        .collect { case (k, Some(v)) => (k, v) }
    val result = cached ++ fromCache ++ received
    replyTo ! result
    replied = true
    self ! PoisonPill
  }


}
