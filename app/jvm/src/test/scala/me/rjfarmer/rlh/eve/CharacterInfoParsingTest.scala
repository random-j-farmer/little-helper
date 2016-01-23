package me.rjfarmer.rlh.eve

import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestActorRef
import akka.util.Timeout
import com.typesafe.config.ConfigFactory
import me.rjfarmer.rlh.api.CharacterInfo
import me.rjfarmer.rlh.eve.EveCharacterInfoApi.CharacterInfoXml
import spray.http.Uri
import utest._

import scala.io.Source
import scala.util.Success

object CharacterInfoParsingTest extends TestSuite {

  implicit val testSystem = {
    val config = ConfigFactory.parseString(
      """
        |akka.loggers = [akka.testkit.TestEventListener]
      """.stripMargin)
    ActorSystem("testsystem", config)
  }

  implicit val timeout = Timeout(1000L, TimeUnit.MILLISECONDS)

  val tests = TestSuite {
    'emptyEmploymentHistory {
      val eveCharacterInfo = TestActorRef[EveCharacterInfoApi]
      val xml = Source.fromURL(getClass.getClassLoader.getResource("empty_employment_history.xml")).mkString

      val query = Uri.Query("characterID" -> 666L.toString)
      val future = eveCharacterInfo ? CharacterInfoXml(query, xml)
      val Success(ci: CharacterInfo) = future.value.get

      assert(147078184 == ci.characterID)
      assert("Forhotea Corp" == ci.characterName)
      assert(Vector() == ci.employmentHistory)
      ci
    }
  }

}
