package me.rjfarmer.rlh.shared

import utest._

object DScanLineCheckTest extends TestSuite {

  val tests = TestSuite {

    'testDistances {
      assert(DScanLineCheck.isValidDScanLine("some name\tRaven\t-"))
      assert(DScanLineCheck.isValidDScanLine("some name\tRaven\t2000 m"))
      assert(DScanLineCheck.isValidDScanLine("some name\tRaven\t1.0 AU"))
      assert(DScanLineCheck.isValidDScanLine("some name\tRaven\t6,666 km"))
    }

    'testEmptyName {
      assert(DScanLineCheck.isValidDScanLine("\tRaven\t-"))
    }

    'testEmptyType {
      assert(! DScanLineCheck.isValidDScanLine("some name\t\t-"))
    }

    'testLessThan2Tabs {
      assert(! DScanLineCheck.isValidDScanLine("some name\t-"))
    }

    'testMoreThan2Tabs {
      assert(! DScanLineCheck.isValidDScanLine("some name\tRaven\tXXX\t-"))
    }
  }

}
