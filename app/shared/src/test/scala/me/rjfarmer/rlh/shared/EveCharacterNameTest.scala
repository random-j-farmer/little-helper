package me.rjfarmer.rlh.shared

import utest._

object EveCharacterNameTest extends TestSuite {

  val tests = TestSuite {

    'valid {
      'minimal {
        assert(EveCharacterName.isValidCharacterName("a b"))
      }
      'twoParts {
        assert(EveCharacterName.isValidCharacterName("Rixx Javix"))
      }
      'threeParts {
        assert(EveCharacterName.isValidCharacterName("Random J Farmer"))
      }
      'maximalMiddle {
        assert(EveCharacterName.isValidCharacterName("Abcdefghij1 Abcdefghij12 Abcdefghij12"))
      }
      'maximalNoMiddle {
        assert(EveCharacterName.isValidCharacterName("AbcdefghijAbcdefghij1234 Abcdefghij12"))
      }
    }

    'invalid {
      'tooshort {
        assert(! EveCharacterName.isValidCharacterName("ab"))
      }
      'onePart {
        assert(! EveCharacterName.isValidCharacterName("abc"))
      }
      'fourParts {
        assert(! EveCharacterName.isValidCharacterName("a b c d"))
      }
      'firstOrMiddleTooLong {
        assert(! EveCharacterName.isValidCharacterName("Abcdefghij1 Abcdefghij123 A"))
      }
      'firstTooLong {
        assert(! EveCharacterName.isValidCharacterName("AbcdefghijAbcdefghij12345 A"))
      }
      'lastTooLong {
        assert(! EveCharacterName.isValidCharacterName("A Abcdefghij123"))
      }
      'lastTooLongWithMiddle {
        assert(! EveCharacterName.isValidCharacterName("A B Abcdefghij123"))
      }
      'illegalCharacter {
        assert(! EveCharacterName.isValidCharacterName("{ }"))
      }
      'hyphenStart {
        assert(! EveCharacterName.isValidCharacterName("-A B"))
      }
      'hyphenEnd {
        assert(! EveCharacterName.isValidCharacterName("A B-"))
      }
      'quotStart {
        assert(! EveCharacterName.isValidCharacterName("'A B"))
      }
      'quotEnd {
        assert(! EveCharacterName.isValidCharacterName("A B'"))
      }

    }

  }

}
