package me.rjfarmer.rlh.shared

object EveCharacterName {

  /**
   * eve character name check.
   *
   * See http://community.eveonline.com/support/policies/eve-user-policy/
   *
   * @param name potential character name
   * @return true if valid name, false otherwise
   */

  def isValidCharacterName(name: String): Boolean = {

    (name.length >= 3 && name.length <= 37) && {
      val names = name.split(' ')
      println("name lenghts: " + names.map(_.size).mkString(", "))
      names.length match {
        // firstname lastname
        case 2 =>
          names(0).length <= 24 &&
            names(1).length <= 12 &&
            names.forall(isValidNamePart)
        // firstname middle lastname
        case 3 =>
          (names(0).length + names(1).length + 1) <= 24 &&
            names(2).length <= 12 &&
            names.forall(isValidNamePart)
        case _ => false
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
