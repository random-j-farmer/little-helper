package me.rjfarmer.rlh.retriever

import akka.actor.ActorRef
import spray.http.Uri
import utest._


final case class TestRetrievable (key: String)
  extends Retrievable[String] {

  override def httpGetUri: Uri = Uri(path = Uri.Path("/xxx"), query = Uri.Query("key" -> key))

  override def replyTo: ActorRef = ???
}

object RetrieveQueueTest extends TestSuite {

  val tests = TestSuite {
    'fgEnqueueDequeue {
      val q = RetrieveQueue[String]()
      q.enqueue(0)(TestRetrievable("random j farmer"))
      q.enqueue(1)(TestRetrievable("random j farmer"))
      q.enqueue(0)(TestRetrievable("some other dude"))

      assert("random j farmer" == q.dequeueOption.get.key)
      assert("some other dude" == q.dequeueOption.get.key)
      assert("random j farmer" == q.dequeueOption.get.key)

      assert(q.dequeueOption.isEmpty)
    }
  }
}
