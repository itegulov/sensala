import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._
import com.typesafe.sbt.gzip.Import._
import com.typesafe.sbt.web.Import._
import com.typesafe.sbt.digest.Import._
import sbtassembly._
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._
import Dependencies._

object SensalaBuild {
  lazy val commonSettings = Seq(
    organization := "",
    version := "0.1",
    scalaVersion := "2.12.8",
    scalacOptions := Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:postfixOps",
      "-language:implicitConversions",
      "-language:higherKinds",
      "-language:experimental.macros",
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
    sbt.Keys.test in assembly := {},
    scalafmtOnCompile := true
  )

  lazy val core = Project(id = "core", base = file("core"))
    .settings(commonSettings)
    .settings(name := "sensala-core")
    .settings(
      libraryDependencies ++= commonDependencies ++ Seq(
        catsCore,
        catsEffect,
        catsMtl,
        monix,
        extjwnl,
        extjwnlDataWn31,
        jverbnet,
        catsLawsTest,
        catsLawsTestkitTest,
        scavenger
      )
    )
    .dependsOn(models, shared % "compile->compile;test->test")

  lazy val parser = Project(id = "parser", base = file("parser"))
    .settings(commonSettings)
    .settings(name := "sensala-parser")
    .settings(
      libraryDependencies ++= commonDependencies ++ Seq(
        stanfordNlp,
        stanfordNlpModelsEnglish,
        stanfordNlpModelsGerman,
        jaxbImpl,
        jaxbCore,
        javaxActivation
      )
    )
    .dependsOn(models, shared % "compile->compile;test->test")

  lazy val shared = Project(id = "shared", base = file("shared"))
    .settings(commonSettings)
    .settings(name := "sensala-shared")
    .settings(
      libraryDependencies ++= commonDependencies ++ Seq(
        catsCore,
        catsEffect,
        catsMtl,
        catsLawsTest,
        catsLawsTestkitTest,
        monix
      )
    )
    .dependsOn(models)

  lazy val commandLine = Project(id = "cli", base = file("cli"))
    .settings(commonSettings)
    .settings(
      name := "sensala-cli",
      mainClass in assembly := Some("sensala.CLI"),
      fullRunInputTask(sensala, Runtime, "sensala.CLI"),
      trapExit in sensala := true,
      fork in sensala := false,
      traceLevel in sensala := 0,
      libraryDependencies ++= commonDependencies ++ Seq(
        scopt
      )
    )
    .dependsOn(core, parser)

  lazy val backend = Project(id = "backend", base = file("backend"))
    .settings(commonSettings)
    .settings(name := "sensala-backend")
    .settings(
      pipelineStages := Seq(digest, gzip),
      mainClass in assembly := Some("sensala.web.Server"),
      assemblyMergeStrategy in assembly := {
        case "logback.xml" =>
          MergeStrategy.first
        case PathList("org", "scalatools", "testing", xs @ _*) =>
          MergeStrategy.first
        case x =>
          val oldStrategy = (assemblyMergeStrategy in assembly).value
          oldStrategy(x)
      },
      libraryDependencies ++= commonDependencies ++ http4sDependencies ++ Seq(
        scopt
      )
    )
    .dependsOn(core, parser, models)

  lazy val models = Project(id = "models", base = file("models"))
      .settings(name := "sensala-models")
      .settings(
        libraryDependencies ++= circeDependencies.value
      )

  lazy val root = Project(id = "sensala", base = file("."))
    .aggregate(
      core,
      parser,
      shared,
      commandLine,
      models,
      backend
    )
    .dependsOn(commandLine)
    .settings(commonSettings)
    .settings(
      name := "Sensala",
      libraryDependencies ++= commonDependencies
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
