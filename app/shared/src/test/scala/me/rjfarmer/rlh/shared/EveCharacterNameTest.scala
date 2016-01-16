package me.rjfarmer.rlh.shared

import utest._

object EveCharacterNameTest extends TestSuite {

  val tests = TestSuite {

    'valid {
      'minimal {
        assert(EveCharacterName.isValidCharacterName("a b"))
      }
      'singlePart {
        assert(EveCharacterName.isValidCharacterName("Mynxee"))
      }
      'twoParts {
        assert(EveCharacterName.isValidCharacterName("Rixx Javix"))
      }
      'threeParts {
        assert(EveCharacterName.isValidCharacterName("Random J Farmer"))
      }
      'alsoLegal {
        assert(EveCharacterName.isValidCharacterName("Wolf SteinerDavion"))
      }
    }

    'invalid {
      'tooshort {
        assert(! EveCharacterName.isValidCharacterName("ab"))
      }
      'fourParts {
        assert(! EveCharacterName.isValidCharacterName("a b c d"))
      }
      'tooLong {
        assert(! EveCharacterName.isValidCharacterName("AbcdefghijAbcdefghijAbcdefghij12345678"))
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
