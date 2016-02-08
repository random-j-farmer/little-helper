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
      'alsoLegalFromJitaLocal {
        for (name <- Seq("Mr. Burke", "Ol' Farmer McNinja", "Pill' Cosby", "Secondary' Target", "Moons Over My Hammy")) {
          assert(EveCharacterName.isValidCharacterName(name))
        }
      }
    }

    'invalid {
      'tooshort {
        assert(!EveCharacterName.isValidCharacterName("ab"))
      }
      'fourParts {
        // it seems 4 name parts are ok as well - "Moons over my Hammy" from jita local
        assert(EveCharacterName.isValidCharacterName("a b c d"))
      }
      'tooLong {
        assert(!EveCharacterName.isValidCharacterName("AbcdefghijAbcdefghijAbcdefghij12345678"))
      }
      'illegalCharacter {
        assert(!EveCharacterName.isValidCharacterName("{ }"))
      }
      'hyphenStart {
        assert(!EveCharacterName.isValidCharacterName("-A B"))
      }
      'hyphenEnd {
        assert(!EveCharacterName.isValidCharacterName("A B-"))
      }
      'quotStart {
        assert(!EveCharacterName.isValidCharacterName("'A B"))
      }
      'quotEnd {
        assert(!EveCharacterName.isValidCharacterName("A B'"))
      }

    }

  }

}
