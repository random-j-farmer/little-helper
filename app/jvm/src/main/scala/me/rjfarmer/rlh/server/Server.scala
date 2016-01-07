package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.routing.FromConfig
import akka.pattern.ask
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.api._
import me.rjfarmer.rlh.eve.CharacterIDApi.{CharacterIDResponse, CharacterIDRequest}
import me.rjfarmer.rlh.eve.CharacterInfoApi.{CharacterInfoResponse, CharacterInfoRequest}
import me.rjfarmer.rlh.eve.ZkStatsApi.{ZkStatsResponse, ZkStatsRequest}
import me.rjfarmer.rlh.eve._
import spray.http.{HttpEntity, MediaTypes}
import spray.routing.SimpleRoutingApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.{Failure, Success}


object Router extends autowire.Server[String, upickle.default.Reader, upickle.default.Writer] {
  def read[Result: upickle.default.Reader](p: String): Result = upickle.default.read(p)

  def write[Result: upickle.default.Writer](r: Result): String = upickle.default.write(r)

}

object Server extends SimpleRoutingApp with Api with RequestTimeout {

  import Boot._

  val eveCharacterID = bootSystem.actorOf(FromConfig.props(EveCharacterIDApi.props), "eveCharacterIDPool")
  val characterID = bootSystem.actorOf(FromConfig.props(CharacterIDApi.props(eveCharacterID)), "characterIDPool")
  val eveCharacterInfo = bootSystem.actorOf(FromConfig.props(EveCharacterInfoApi.props), "eveCharacterInfoPool")
  val characterInfo = bootSystem.actorOf(FromConfig.props(CharacterInfoApi.props(eveCharacterInfo)), "characterInfoPool")
  val eveZkStats = bootSystem.actorOf(FromConfig.props(RestZkStatsApi.props), "restZkStatsPool")
  val zkStats = bootSystem.actorOf(FromConfig.props(ZkStatsApi.props(eveZkStats)), "zkStatsPool")

  def main(args: Array[String]): Unit = {

    startServer(bootHost, port = bootPort) {
      get {
        pathSingleSlash {
          complete {
            HttpEntity(MediaTypes.`text/html`, Page.skeleton.render)
          }
        } ~
          getFromResourceDirectory("") ~
          getFromResourceDirectory("META-INF/resources")
      } ~
        post {
          path("ajax" / Segments) { s =>
            extract(_.request.entity.asString) { e =>
              complete(Router.route[Api](Server)(autowire.Core.Request(s, upickle.default.read[Map[String, String]](e))))
            }
          }
        }
    }
  }

  implicit val timeoutDuration = bootTimeout.duration

  private def listIds(names: Seq[String]): Future[Seq[CharacterIDAndName]] = {
    ask(characterID, CharacterIDRequest(names, Seq(), Seq()))
      .asInstanceOf[Future[CharacterIDResponse]]
      .map(resp => resp.fullResult.get.filter(ian => ian.characterID != 0L))
  }

  private def characterInfo(id: Long): Future[CharacterInfo] = {
    ask(characterInfo, CharacterInfoRequest(id, Seq()))
      .asInstanceOf[Future[CharacterInfoResponse]]
      .map(resp => resp.result.get)
  }

  private def zkStats(id: Long): Future[ZkStats] = {
    ask(zkStats, ZkStatsRequest(id, Seq()))
      .asInstanceOf[Future[ZkStatsResponse]]
      .map(resp => resp.stats.get)
  }

  def listCharacters(names: Seq[String]): Future[Seq[CharInfo]] = {

    val idsFuture = listIds(names)
    val result = Promise[Seq[CharInfo]]()
    idsFuture.onComplete {
      case Success(ids) =>
        val f1 = Future.sequence(ids.map((ian) => characterInfo(ian.characterID)))
        val f2 = Future.sequence(ids.map((ian) => zkStats(ian.characterID)))
        f1.zip(f2).onComplete {
          case Success(Pair(infos, zkstats)) =>
            result.success(infos.zip(zkstats)
              .map { pair => CharInfo(pair._1, pair._2)}
              .sorted(ByActiveAndDestroyed))
          case Failure(ex) =>
            result.failure(ex)
        }
      case Failure(ex) =>
        result.failure(ex)
    }
    result.future
  }

}


object Boot extends RequestTimeout {

  val bootConfig = ConfigFactory.load()
  val bootHost = bootConfig.getString("http.host")
  val bootPort = bootConfig.getInt("http.port")

  implicit val bootSystem = ActorSystem("little-helper", bootConfig)
  implicit val bootTimeout = requestTimeout(bootConfig)

}

object ByActiveAndDestroyed extends Ordering[CharInfo] {
  override def compare(x: CharInfo, y: CharInfo): Int = {
    val zk1 = x.zkStats
    val zk2 = y.zkStats
    val act = (zk2.activepvp.kills > 0).compareTo(zk1.activepvp.kills > 0) // reverse
    act match {
      case 0 => zk2.lastMonths.shipsDestroyed.compareTo(zk1.lastMonths.shipsDestroyed)
      case _ => act
    }
  }
}

trait RequestTimeout {
  import scala.concurrent.duration._

  def requestTimeout(config: Config): Timeout = {
    val t = config.getString("spray.can.server.request-timeout")
    val d = Duration(t)
    FiniteDuration(d.length, d.unit)
  }
}



