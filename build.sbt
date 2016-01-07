import sbt.Keys._
import com.lihaoyi.workbench.Plugin._
import spray.revolver.AppProcess
import spray.revolver.RevolverPlugin.Revolver

val akkaVersion = "2.3.14"
val sprayVersion = "1.3.3"


val app = crossProject.settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "scalatags" % "0.5.3",
    "com.lihaoyi" %%% "upickle" % "0.3.6",
    "com.lihaoyi" %%% "utest" % "0.3.1",
    "com.lihaoyi" %%% "autowire" % "0.2.5"
  ),
  scalaVersion := "2.11.7"
).jsSettings(
  workbenchSettings: _*
).jsSettings(
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.2"
  ),
  bootSnippet := "me.rjfarmer.rlh.client.LittleHelper().main(document.getElementById('rlhMain'));"
).jvmSettings(
  Revolver.settings: _*
).jvmSettings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "ch.qos.logback"      %  "logback-classic" % "1.1.3",
    "io.spray" %% "spray-can" % sprayVersion,
    "io.spray" %% "spray-routing" % sprayVersion,
    "io.spray" %% "spray-caching" % sprayVersion,
    "org.webjars" % "pure" % "0.6.0",
    "org.json4s" %% "json4s-jackson" % "3.3.0",
    "com.h2database" % "h2" % "1.4.190",
    "com.lihaoyi" % "ammonite-repl" % "0.5.2" % "test" cross CrossVersion.full
  )
)

val appJS = app.js
val appJVM = app.jvm.settings(
  (mainClass in Compile) := Some("me.rjfarmer.rlh.server.Server"),
  (resources in Compile) += {
    (fastOptJS in(appJS, Compile)).value
    (artifactPath in(appJS, Compile, fastOptJS)).value
  },
  (initialCommands in (Test, console)) := """ammonite.repl.Main.run("")"""
)
