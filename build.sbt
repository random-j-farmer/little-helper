import sbt.Keys._
import com.lihaoyi.workbench.Plugin._
import spray.revolver.AppProcess
import spray.revolver.RevolverPlugin.Revolver

val app = crossProject.settings(
  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "scalatags" % "0.5.2",
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
  bootSnippet := "simple.Client().main(document.getElementById('contents'))"
).jvmSettings(
  Revolver.settings: _*
).jvmSettings(
  libraryDependencies ++= Seq(
    "io.spray" %% "spray-can" % "1.3.3",
    "io.spray" %% "spray-routing" % "1.3.3",
    "com.typesafe.akka" %% "akka-actor" % "2.3.11",
    "com.lihaoyi" % "ammonite-repl" % "0.4.8" % "test" cross CrossVersion.full
  )
)

lazy val appJS = app.js
lazy val appJVM = app.jvm.settings(
  (resources in Compile) += {
    (fastOptJS in(appJS, Compile)).value.data
    (artifactPath in(appJS, Compile, fastOptJS)).value
  },
  (initialCommands in (Test, console)) := """ammonite.repl.Repl.run("")"""
)
