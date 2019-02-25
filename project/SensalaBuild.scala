import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._
import play.sbt._
import play.sbt.PlayImport._
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
    .dependsOn(core)

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

  lazy val webServer = Project(id = "web-server", base = file("web-server"))
    .settings(commonSettings)
    .settings(name := "sensala-web-server")
    .settings(
      scalaJSProjects := Seq(webClient),
      pipelineStages in Assets := Seq(scalaJSPipeline),
      pipelineStages := Seq(digest, gzip),
      compile in Compile := (compile in Compile).dependsOn(scalaJSPipeline).value,
      mainClass in assembly := Some("play.core.server.ProdServerStart"),
      fullClasspath in assembly += Attributed.blank(PlayKeys.playPackageAssets.value),
      assemblyMergeStrategy in assembly := {
        case manifest if manifest.contains("MANIFEST.MF") =>
          // We don't need manifest files since sbt-assembly will create
          // one with the given settings
          MergeStrategy.discard
        case PathList("org", "scalatools", "testing", xs @ _*) =>
          MergeStrategy.first
        case referenceOverrides if referenceOverrides.contains("reference-overrides.conf") =>
          // Keep the content for all reference-overrides.conf files
          MergeStrategy.concat
        case "application.conf" => MergeStrategy.concat
        case "logback.xml"      => MergeStrategy.first
        case x =>
          val oldStrategy = (assemblyMergeStrategy in assembly).value
          oldStrategy(x)
      },
      libraryDependencies ++= commonDependencies ++ Seq(
        guice,
        webjarBootstrap,
        webjarJquery,
        webjarPopper,
        webjarD3js,
        webjarDagreD3,
        scalaJsScripts
      )
    )
    .dependsOn(core, parser, webSharedJvm)
    .enablePlugins(PlayScala)

  lazy val webHttp4sServer = Project(id = "web-http4s-server", base = file("web-http4s-server"))
    .settings(commonSettings)
    .settings(name := "sensala-web-http4s-server")
    .settings(
      scalaJSProjects := Seq(webClient),
      pipelineStages in Assets := Seq(scalaJSPipeline),
      pipelineStages := Seq(digest, gzip),
      compile in Compile := (compile in Compile).dependsOn(scalaJSPipeline).value,
      mainClass in assembly := Some("sensala.web.Server"),
      // Allows to read the generated JS on client
      resources in Compile += (fastOptJS in (webClient, Compile)).value.data,
      // Lets the backend to read the .map file for js
      resources in Compile += (fastOptJS in (webClient, Compile)).value
        .map((x: sbt.File) => new File(x.getAbsolutePath + ".map"))
        .data,
      // Lets the server read the jsdeps file
      (managedResources in Compile) += (artifactPath in (webClient, Compile, packageJSDependencies)).value,
      // This settings makes reStart to rebuild if a scala.js file changes on the client
      watchSources ++= (watchSources in webClient).value,
      libraryDependencies ++= commonDependencies ++ http4sDependencies ++ Seq(
        guice,
        webjarBootstrap,
        webjarJquery,
        webjarPopper,
        webjarD3js,
        webjarDagreD3,
        scalaJsScripts
      )
    )
    .dependsOn(core, parser, webSharedJvm)
    .enablePlugins(SbtTwirl, SbtWeb)

  lazy val webClient = Project(id = "web-client", base = file("web-client"))
    .settings(commonSettings)
    .settings(name := "sensala-web-client")
    .settings(
      // Build a js dependencies file
      skip in packageJSDependencies := false,
      jsEnv := new org.scalajs.jsenv.nodejs.NodeJSEnv(), // Put the jsdeps file on a place reachable for the server
      crossTarget in (Compile, packageJSDependencies) := (resourceManaged in Compile).value,
      scalacOptions += "-P:scalajs:sjsDefinedByDefault",
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= commonDependencies ++ Seq(
        "org.scala-js" %%% "scalajs-dom"    % "0.9.6",
        "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
        "com.typesafe.play" %%% "play-json" % "2.6.10"
      )
    )
    .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
    .dependsOn(webSharedJs)

  lazy val webShared =
    (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("web-shared"))
      .settings(name := "sensala-web-shared")
      .settings(
        libraryDependencies ++= Seq(
          "org.julienrf" %%% "play-json-derived-codecs" % "5.0.0"
        )
      )
      .jsConfigure(_.enablePlugins(ScalaJSPlugin, ScalaJSWeb))

  lazy val webSharedJvm = webShared.jvm
  lazy val webSharedJs  = webShared.js

  lazy val root = Project(id = "sensala", base = file("."))
    .aggregate(
      core,
      parser,
      commandLine,
      webSharedJvm,
      webSharedJs,
      webServer,
      webHttp4sServer,
      webClient
    )
    .dependsOn(core, parser % "compile->compile;test->test", commandLine)
    .settings(commonSettings)
    .settings(
      name := "Sensala",
      libraryDependencies ++= commonDependencies
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
