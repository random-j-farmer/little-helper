package me.rjfarmer.rlh.api

import scala.concurrent.Future


sealed class XmlApiResult

final case class CharacterInfo(characterID: String, characterName: String,
                               race: String, bloodline: String, ancestry: String,
                               corporationID: String, corporation: String, corporationDate: String,
                               allianceID: Option[String], alliance: Option[String], allianceDate: Option[String],
                               securityStatus: Double) extends XmlApiResult

final case class CharacterIDAndName(characterID: String, characterName: String) extends XmlApiResult

trait Api {

  def listCharacters(names: Seq[String]): Future[Seq[CharacterInfo]]

}
