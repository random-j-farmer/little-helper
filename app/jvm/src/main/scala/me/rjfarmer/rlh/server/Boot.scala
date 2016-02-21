package me.rjfarmer.rlh.server

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory}
import me.rjfarmer.rlh.retriever.PriorityConfig
import me.rjfarmer.rlh.shared.{ClientConfig, SharedConfig}
import org.ehcache.CacheManagerBuilder
import org.ehcache.config.xml.XmlConfiguration
import spray.can.Http

object BootLoader {

  var testEnvironment: Boolean = false

  def cacheManagerConfiguration = {
    val fn = if (testEnvironment) "little-cache-test.xml" else "little-cache.xml"
    new XmlConfiguration(getClass.getClassLoader.getResource(fn))
  }

}

object Boot extends RequestTimeout {

  val bootConfig = ConfigFactory.load()
  val bootHost = bootConfig.getString("http.host")
  val bootPort = bootConfig.getInt("http.port")

  val cacheManager = CacheManagerBuilder.newCacheManager(BootLoader.cacheManagerConfiguration)

  import collection.JavaConversions._

  val priorityConfig = PriorityConfig(
    bootConfig.getIntList("little-helper.priorities-by-size").toVector.map(_.toInt),
    bootConfig.getIntList("little-helper.promote-stales").toVector.map(_.toInt),
    bootConfig.getInt("little-helper.stale-priority-offset")
  )

  cacheManager.init()

  implicit val bootSystem = ActorSystem("little-helper", bootConfig)
  implicit val ajaxFutureTimeout = requestTimeout(bootConfig, "little-helper.ajax-future-timeout")
  val restTimeout = requestTimeout(bootConfig, "little-helper.rest-timeout")
  val staleIfOlderThan = requestTimeout(bootConfig, "little-helper.stale-if-older-than")
  SharedConfig.client = ClientConfig(BuildInfo.version, staleIfOlderThan.duration.toMillis)

  object CacheManagerShutdownHook extends Thread {

    override def run(): Unit = {
      println("Shutting down actor system")
      bootSystem.shutdown()
      println("Shutting down cache manager")
      cacheManager.close()
    }

  }
}

trait RequestTimeout {
  import scala.concurrent.duration._

  def requestTimeout(config: Config, configKey: String): Timeout = {
    val t = config.getString(configKey)
    val d = Duration(t)
    FiniteDuration(d.length, d.unit)
  }
}

trait ShutdownIfNotBound {
  import scala.concurrent.{ExecutionContext, Future}

  def shutdownIfNotBound(f: Future[Any])
                        (implicit system: ActorSystem, ec: ExecutionContext) = {
    f.mapTo[Http.Event].map {
      case Http.Bound(address) =>
        println(s"Interface bound to $address")

      case Http.CommandFailed(cmd) =>
        println(s"nterface could not bind: ${cmd.failureMessage}, shutting down.")
        system.shutdown()
    }.recover {
      case e: Throwable =>
        println(s"Unexpexted error binding to HTTP: ${e.getMessage}, shutting down.")
        system.shutdown()
    }
  }
}



