import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._

object SensalaBuild {
  lazy val commonSettings = Seq(
    organization := "",
    version := "0.1",
    scalaVersion := "2.12.3",
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
    scalacOptions in (Compile, console) -= "-Ywarn-unused-import",
    scalacOptions in (Compile, doc) ++= Seq("-diagrams", "-implicits"),
    scalacOptions in Test ++= Seq("-Yrangepos")
  )

  lazy val commonDeps = Seq(
    libraryDependencies ++= Seq(
      "ch.qos.logback"             % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging"  % "3.5.0",
      "org.scalatest"              %% "scalatest"      % "3.0.4" % Test
    )
  )

  lazy val core = Project(id = "core", base = file("core"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-core")
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel"              %% "cats-core"      % "1.0.0-MF",
        "org.typelevel"              %% "cats-mtl-core"  % "0.0.2",
        "com.ironcorelabs"           %% "cats-scalatest" % "2.3.0" % Test,
        "org.aossie" %% "scavenger" % "0.2"
      )
    )

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

  lazy val root = Project(id = "sensala", base = file("."))
    .aggregate(core, parser, commandLine)
    .dependsOn(core, parser % "compile->compile;test->test", commandLine)
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "Sensala"
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
