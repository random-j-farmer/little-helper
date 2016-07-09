import sbt.Keys._
import spray.revolver.RevolverPlugin.Revolver

val akkaVersion = "2.3.15"
val sprayVersion = "1.3.3"

val app = crossProject.settings(
  name := "little-helper",
  organization := "me.rjfarmer",

  unmanagedSourceDirectories in Compile +=
    baseDirectory.value / "shared" / "main" / "scala",
  libraryDependencies ++= Seq(
    "com.lihaoyi" %%% "scalatags" % "0.5.5",
    "com.lihaoyi" %%% "upickle" % "0.4.1",
    "com.lihaoyi" %%% "utest" % "0.4.3",
    "com.lihaoyi" %%% "autowire" % "0.2.5"
  ),
  testFrameworks += new TestFramework("utest.runner.Framework"),
  scalaVersion := "2.11.8",
  pomExtra :=
    <url>https://github.com/random-j-farmer/little-helper</url>
      <licenses>
        <license>
          <name>PTS License</name>
          <url>https://github.com/random-j-farmer/little-helper/LICENSE.md</url>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/random-j-farmer/little-helper.git</url>
        <connection>scm:git://github.com/random-j-farmer/little-helper.git</connection>
      </scm>
      <developers>
        <developer>
          <id>random-j-farmer</id>
          <name>Random J Farmer</name>
          <url>https://github.com/random-j-farmer</url>
        </developer>
      </developers>
).enablePlugins(GitVersioning, BuildInfoPlugin, JavaAppPackaging
).jsSettings(
    libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.1"
  )
).jvmSettings(
  Revolver.settings: _*
).jvmSettings(
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  buildInfoPackage := "me.rjfarmer.rlh.server",
  git.useGitDescribe := true,
  git.baseVersion := "0.0.0"
).jvmSettings(
  libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaVersion,
    "com.typesafe.akka" %% "akka-slf4j" % akkaVersion,
    "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
    "ch.qos.logback"      %  "logback-classic" % "1.1.7",
    "io.spray" %% "spray-can" % sprayVersion,
    "io.spray" %% "spray-routing" % sprayVersion,
    "org.spire-math" %% "jawn-ast" % "0.8.4",
    "org.ehcache" % "ehcache" % "3.1.0",
    "com.lihaoyi" % "ammonite-repl" % "0.6.2" % "test" cross CrossVersion.full
  )
)

val appJS = app.js
val appJVM = app.jvm.settings(
  (mainClass in Compile) := Some("me.rjfarmer.rlh.server.Server"),
  artifactPath in (appJS, Compile, fastOptJS) := (resourceManaged in Compile).value / "WEB-ROOT" /
    ((moduleName in fastOptJS).value + "-fastopt.js"),
  (resources in Compile) += {
    (fastOptJS in(appJS, Compile)).value
    (artifactPath in(appJS, Compile, fastOptJS)).value
  },
  (initialCommands in (Test, console)) := """ammonite.repl.Main.run("")"""
)
val appJS2 = app.js.settings(
  artifactPath in fastOptJS := (resourceManaged in appJVM in Compile).value /
    ((moduleName in fastOptJS).value + "-xxx.js")
)


val VersionRegex = "v([0-9]+.[0-9]+.[0-9]+)-?(.*)?".r
git.gitTagToVersionNumber := {
  case VersionRegex(v,"") => Some(v)
  case VersionRegex(v,"SNAPSHOT") => Some(s"$v-SNAPSHOT")
  case VersionRegex(v,s) => Some(s"$v-$s-SNAPSHOT")
  case _ => None
}
