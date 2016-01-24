package me.rjfarmer.rlh.eve

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.testkit.TestActorRef
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import me.rjfarmer.rlh.api.{ZkStats, CharacterInfo}
import me.rjfarmer.rlh.eve.RestZkStatsApi.ZkStatsJson
import me.rjfarmer.rlh.server.BootLoader
import spray.http.Uri
import utest._
import utest.framework.TestSuite

import scala.io.Source
import scala.util.Success

object ZkStatsParsingTest extends TestSuite {

  BootLoader.testEnvironment = true

  implicit val testSystem = {
    val config = ConfigFactory.parseString(
      """
        |akka.loggers = [akka.testkit.TestEventListener]
      """.stripMargin)
    ActorSystem("testsystem", config)
  }

  implicit val timeout = Timeout(1000L, TimeUnit.MILLISECONDS)

  val tests = TestSuite {
    'rixxJavix {
      val restZkStats = TestActorRef[RestZkStatsApi]
      val json = Source.fromURL(getClass.getClassLoader.getResource("rixx_javix_zkstats.json")).mkString

      val uri = Uri(path = Uri.Path("/api/stats/characterID/245073304"))
      val future = restZkStats ? ZkStatsJson(uri, json)
      val Success(zk: ZkStats) = future.value.get

      assert(245073304L == zk.info.id)
      assert("Rixx Javix" == zk.info.name)
      // i deleted the last 3 months in the example json, so the stats for the last 2 months will be zero
      assert(0 == zk.lastMonths.shipsDestroyed)
      assert(0 == zk.lastMonths.shipsLost)
      zk

    }

    'snuggles {
      // this example has no killboard activity, and the id is not in the returned json
      val restZkStats = TestActorRef[RestZkStatsApi]
      val json = Source.fromURL(getClass.getClassLoader.getResource("snuggles.json")).mkString

      val uri = Uri(path = Uri.Path("/api/stats/characterID/95407938"))
      val future = restZkStats ? ZkStatsJson(uri, json)
      val Success(zk: ZkStats) = future.value.get

      // the id is taken from the URL if its not in the payload
      assert(95407938L == zk.info.id)

      // no activity, so zero
      assert(0 == zk.lastMonths.shipsDestroyed)
      assert(0 == zk.lastMonths.shipsLost)
      zk

    }
  }

}
