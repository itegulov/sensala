import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject, _}
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._
import webscalajs.WebScalaJS.autoImport._
import com.typesafe.sbt.gzip.Import._
import com.typesafe.sbt.web.Import._
import com.typesafe.sbt.digest.Import._
import sbtassembly._
import org.scalafmt.sbt.ScalafmtPlugin.autoImport._
import webscalajs.ScalaJSWeb
import Dependencies._
import com.typesafe.sbt.web.SbtWeb
import play.twirl.sbt.SbtTwirl

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
        jwnl,
        catsLawsTest,
        catsLawsTestkitTest,
        scavenger
      )
    )
    .dependsOn(models.jvm, shared % "compile->compile;test->test")

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
    .dependsOn(models.jvm, shared % "compile->compile;test->test")

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
    .dependsOn(models.jvm)

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
      scalaJSProjects := Seq(frontend),
      pipelineStages in Assets := Seq(scalaJSPipeline),
      pipelineStages := Seq(digest, gzip),
      compile in Compile := (compile in Compile).dependsOn(scalaJSPipeline).value,
      mainClass in assembly := Some("sensala.web.Server"),
      // Allows to read the generated JS on client
      resources in Compile += (fastOptJS in (frontend, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (frontend, Compile)).value
        .map((x: sbt.File) => new File(x.getAbsolutePath + ".map"))
        .data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in (frontend, Compile, packageJSDependencies)).value,
      // This settings makes reStart to rebuild if a scala.js file changes on the client
      watchSources ++= (watchSources in frontend).value,
      libraryDependencies ++= commonDependencies ++ http4sDependencies ++ Seq(
        webjarBootstrap,
        webjarJquery,
        webjarPopper,
        webjarD3js,
        webjarDagreD3,
        scalaJsScripts
      )
    )
    .dependsOn(core, parser, models.jvm)
    .enablePlugins(SbtTwirl, SbtWeb)

  lazy val frontend = Project(id = "frontend", base = file("frontend"))
    .settings(commonSettings)
    .settings(name := "sensala-frontend")
    .settings(
      // Build a js dependencies file
      skip in packageJSDependencies := false,
      jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(), // Put the jsdeps file on a place reachable for the server
      crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
      scalacOptions += "-P:scalajs:sjsDefinedByDefault",
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= commonDependencies ++ Seq(
        "org.scala-js" %%% "scalajs-dom"    % "0.9.6",
        "org.singlespaced" %%% "scalajs-d3" % "0.3.4"
      )
    )
    .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
    .dependsOn(models.js)

  lazy val models =
    (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("models"))
      .settings(name := "sensala-models")
      .settings(
        libraryDependencies ++= circeDependencies.value
      )
      .jsConfigure(_.enablePlugins(ScalaJSPlugin, ScalaJSWeb))

  lazy val root = Project(id = "sensala", base = file("."))
    .aggregate(
      core,
      parser,
      shared,
      commandLine,
      models.js,
      models.jvm,
      backend,
      frontend
    )
    .dependsOn(commandLine)
    .settings(commonSettings)
    .settings(
      name := "Sensala",
      libraryDependencies ++= commonDependencies
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
