import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._
import play.sbt._
import play.sbt.PlayImport._

object SensalaBuild {
  lazy val commonSettings = Seq(
    organization := "",
    version := "0.1",
    scalaVersion := "2.12.4",
    scalacOptions := Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-dead-code",
      "-Ypartial-unification",
      "-Xfuture",
      "-Xexperimental"
    ),
    resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
    scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    scalacOptions in Test ++= Seq("-Yrangepos"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
  )

  lazy val commonDeps = Seq(
    libraryDependencies ++= Seq(
      "ch.qos.logback"             % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging"  % "3.5.0",
      "org.scalatest"              %% "scalatest"      % "3.0.4" % Test
    )
  )

  lazy val conceptNet = Project(id = "concept-net", base = file("concept-net"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-concept-net")
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-ahc-ws-standalone"  % "1.1.2",
        "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.2",
        "net.sf.ehcache"    % "ehcache"                  % "2.10.4"
      )
    )

  lazy val core = Project(id = "core", base = file("core"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-core")
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel"    %% "cats-core"      % "1.0.1",
        "org.typelevel"    %% "cats-mtl-core"  % "0.2.1",
        "org.atnos"        %% "eff"            % "5.0.0-RC1-20180125204657-3712000",
        "com.ironcorelabs" %% "cats-scalatest" % "2.3.1" % Test,
        "org.aossie"       %% "scavenger"      % "0.2.1-SNAPSHOT"
      )
    )
    .dependsOn(conceptNet)

  lazy val parser = Project(id = "parser", base = file("parser"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-parser")
    .settings(
      libraryDependencies ++= Seq(
        "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
        "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models"
      )
    )
    .dependsOn(core)

  lazy val commandLine = Project(id = "cli", base = file("cli"))
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "sensala-cli",
      mainClass in assembly := Some("sensala.CLI"),
      fullRunInputTask(sensala, Runtime, "sensala.CLI"),
      trapExit in sensala := true,
      fork in sensala := false,
      traceLevel in sensala := 0,
      libraryDependencies ++= Seq(
        "com.github.scopt" %% "scopt" % "3.7.0"
      )
    )
    .dependsOn(core, parser)
  
  lazy val web = Project(id = "web", base = file("web"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-web")
    .settings(
      libraryDependencies ++= Seq(
        guice
      )
    )
    .dependsOn(core, parser)
    .enablePlugins(PlayScala)

  lazy val root = Project(id = "sensala", base = file("."))
    .aggregate(core, parser, commandLine)
    .dependsOn(core, parser % "compile->compile;test->test", commandLine)
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "Sensala"
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
