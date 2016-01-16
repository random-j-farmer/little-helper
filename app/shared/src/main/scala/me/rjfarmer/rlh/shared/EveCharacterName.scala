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
      name.split(' ') match {
        case Array(name) =>
          name.length <= 24 &&
          isValidNamePart(name)
        case names @ Array(firstName, lastName) =>
          firstName.length <= 24 &&
            lastName.length <= 12 &&
            names.forall(isValidNamePart)
        case names @ Array(firstName, middleName, lastName) =>
          (firstName.length + middleName.length + 1) <= 24 &&
            lastName.length <= 12 &&
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
