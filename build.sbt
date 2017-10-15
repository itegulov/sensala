name := "Sensala"

organization := ""

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Xdisable-assertions")

scalacOptions in (Compile, doc) ++= Seq("-diagrams","-implicits")

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases", "public").map(Resolver.sonatypeRepo)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "3.7.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.6.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0",
  "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" classifier "models"
)

licenses := Seq("CC BY-NC-SA" -> url("http://creativecommons.org/licenses/by-nc-sa/4.0/"))

homepage := Some(url("http://TODO"))

val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")

lazy val project = Project(id = "sensala", base = file("."))
  .settings(
    mainClass in assembly := Some("sensala.CLI"),
    fullRunInputTask(sensala, Runtime, "sensala.CLI"),
    trapExit in sensala := true,
    fork in sensala := false,
    traceLevel in sensala := 0
  )
