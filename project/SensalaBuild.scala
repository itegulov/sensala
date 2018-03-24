import sbt._
import sbt.Keys._
import sbtassembly.AssemblyKeys._
import play.sbt._
import play.sbt.PlayImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._
import webscalajs.WebScalaJS.autoImport._
import com.typesafe.sbt.gzip.Import._
import com.typesafe.sbt.web.Import._
import com.typesafe.sbt.digest.Import._
import sbtassembly._
import webscalajs.ScalaJSWeb

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
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
    sbt.Keys.test in assembly := {}
  )

  lazy val commonDeps = Seq(
    libraryDependencies ++= Seq(
      "ch.qos.logback"             % "logback-classic" % "1.2.3",
      "com.typesafe.scala-logging" %% "scala-logging"  % "3.8.0",
      "org.scalatest"              %% "scalatest"      % "3.0.5" % Test
    )
  )

  lazy val conceptNet = Project(id = "concept-net", base = file("concept-net"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-concept-net")
    .settings(
      libraryDependencies ++= Seq(
        "com.typesafe.play" %% "play-ahc-ws-standalone"  % "1.1.6",
        "com.typesafe.play" %% "play-ws-standalone-json" % "1.1.6",
        "org.ehcache"       % "ehcache"                  % "3.4.0"
      )
    )

  lazy val core = Project(id = "core", base = file("core"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-core")
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel"    %% "cats-core"      % "1.0.1",
        "org.typelevel"    %% "cats-mtl-core"  % "0.2.1",
        "org.atnos"        %% "eff"            % "5.1.0",
        "net.sf.jwordnet"  % "jwnl"            % "1.3.3",
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
        "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1",
        "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models"
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

  lazy val webServer = Project(id = "web-server", base = file("web-server"))
    .settings(commonSettings ++ commonDeps)
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
      libraryDependencies ++= Seq(
        guice,
        "org.webjars"     % "bootstrap"        % "4.0.0",
        "org.webjars"     % "jquery"           % "3.3.1",
        "org.webjars.npm" % "popper.js"        % "1.13.0",
        "org.webjars"     % "d3js"             % "3.5.17",
        "org.webjars.npm" % "dagre-d3"         % "0.4.17",
        "com.vmunier"     %% "scalajs-scripts" % "1.1.1"
      )
    )
    .dependsOn(core, parser, webSharedJvm)
    .enablePlugins(PlayScala)

  lazy val webClient = Project(id = "web-client", base = file("web-client"))
    .settings(commonSettings ++ commonDeps)
    .settings(name := "sensala-web-client")
    .settings(
      scalacOptions += "-P:scalajs:sjsDefinedByDefault",
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom"    % "0.9.4",
        "org.singlespaced" %%% "scalajs-d3" % "0.3.4",
        "com.typesafe.play" %%% "play-json" % "2.6.8"
      )
    )
    .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
    .dependsOn(webSharedJs)

  lazy val webShared = (crossProject.crossType(CrossType.Pure) in file("web-shared"))
    .settings(name := "sensala-web-shared")
    .settings(
      libraryDependencies ++= Seq(
        "org.julienrf" %%% "play-json-derived-codecs" % "4.0.0"
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
      conceptNet,
      webSharedJvm,
      webSharedJs,
      webServer,
      webClient
    )
    .dependsOn(core, parser % "compile->compile;test->test", commandLine)
    .settings(commonSettings ++ commonDeps)
    .settings(
      name := "Sensala"
    )

  val sensala = InputKey[Unit]("sensala", "A Dynamic Semantics Framework")
}
