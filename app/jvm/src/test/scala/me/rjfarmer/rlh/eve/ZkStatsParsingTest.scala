package me.rjfarmer.rlh.eve

import utest._

import scala.io.Source

object ZkStatsParsingTest extends TestSuite {


  val tests = TestSuite {
    'rixxJavix {
      val json = Source.fromURL(getClass.getClassLoader.getResource("rixx_javix_zkstats.json")).mkString

      val zk = ZkStatsBodyParser.parseBody(-1L, json)

      assert(245073304L == zk.info.id)
      assert("Rixx Javix" == zk.info.name)
      // i deleted the last 3 months in the example json, so the stats for the last 2 months will be zero
      assert(0 == zk.lastMonths.shipsDestroyed)
      assert(0 == zk.lastMonths.shipsLost)
      zk
    }

    'snuggles {
      // this example has no killboard activity, and the id is not in the returned json
      val json = Source.fromURL(getClass.getClassLoader.getResource("snuggles.json")).mkString

      val zk = ZkStatsBodyParser.parseBody(95407938L, json)

      // the id is taken from the URL if its not in the payload
      assert(95407938L == zk.info.id)

      // no activity, so zero
      assert(0 == zk.lastMonths.shipsDestroyed)
      assert(0 == zk.lastMonths.shipsLost)
      zk

    }
  }

}
