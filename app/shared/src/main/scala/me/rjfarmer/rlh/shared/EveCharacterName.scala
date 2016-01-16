package me.rjfarmer.rlh.shared

object EveCharacterName {

  /**
   * eve character name check.
   *
   * See http://community.eveonline.com/support/policies/eve-user-policy/
   *
   * Alas, the name part length stuff seems bogus.
   * So we only check for max three parts, legal characters and total length.
   *
   * @param name potential character name
   * @return true if valid name, false otherwise
   */

  def isValidCharacterName(name: String): Boolean = {

    (name.length >= 3 && name.length <= 37) && {
      name.split(' ') match {
        case Array(name) =>
          isValidNamePart(name)
        case names @ Array(_, _) =>
          names.forall(isValidNamePart)
        case names @ Array(_, _, _) =>
          names.forall(isValidNamePart)
        case _ =>
          false
      }
    }
  }

  private val validCharacters = """^[a-zA-Z0-9-']*$""".r

  private def isValidNamePart(part: String) = {
    part.length >= 1 &&
      (part match {
        case validCharacters(_*) => true
        case _ => false
      }) &&
      part.head != '-' &&
      part.head != ''' &&
      part.last != '-' &&
      part.last != '''

  }

}
