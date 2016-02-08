package me.rjfarmer.rlh.shared

object EveCharacterName {

  /**
   * eve character name check.
   *
   * See http://community.eveonline.com/support/policies/eve-user-policy/
   *
   * Alas, the name part length stuff seems bogus.
   * Max number of name parts (3) is also bogus.
   * So we only check for legal characters and total length.
   *
   * @param name potential character name
   * @return true if valid name, false otherwise
   */

  def isValidCharacterName(name: String): Boolean = {

    (name.length >= 3 && name.length <= 37) &&
      isValidFirstOrLast(name.head) &&
      isValidFirstOrLast(name.last) &&
      name.split(' ').forall(isValidNamePart)
  }

  private val validCharacters = """^[a-zA-Z0-9-'.]*$""".r

  private def isValidFirstOrLast(ch: Char): Boolean = {
    ch != '-' && ch != ''' && ch != '.'
  }

  private def isValidNamePart(part: String) = {
    part.length >= 1 &&
      (part match {
        case validCharacters(_*) => true
        case _ => false
      })

  }

}
