package me.rjfarmer.rlh.eve

import utest._

import scala.io.Source

object CharacterInfoParsingTest extends TestSuite {

  val tests = TestSuite {
    'emptyEmploymentHistory {
      val xml = Source.fromURL(getClass.getClassLoader.getResource("empty_employment_history.xml")).mkString

      val ci = CharacterInfoBodyParser.parseBody(-1L, xml)

      assert(147078184 == ci.characterID)
      assert("Forhotea Corp" == ci.characterName)
      assert(Vector() == ci.employmentHistory)
      ci
    }
  }

}
