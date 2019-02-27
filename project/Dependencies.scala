import sbt._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

object Dependencies {
  val catsVersion        = "1.6.0"
  val catsMtlVersion     = "0.4.0"
  val circeVersion       = "0.11.1"
  val http4sVersion      = "0.19.0"
  val stanfordNlpVersion = "3.9.1"

  val catsCore            = "org.typelevel" %% "cats-core"     % catsVersion
  val catsLawsTest        = "org.typelevel" %% "cats-laws"     % catsVersion % Test
  val catsLawsTestkitTest = "org.typelevel" %% "cats-testkit"  % catsVersion % Test
  val catsEffect          = "org.typelevel" %% "cats-effect"   % "1.2.0"
  val catsMtl             = "org.typelevel" %% "cats-mtl-core" % catsMtlVersion
  val catsMtlLawsTest     = "org.typelevel" %% "cats-mtl-laws" % catsMtlVersion % Test

  val http4sBlazeClient = "org.http4s" %% "http4s-blaze-client" % http4sVersion
  val http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % http4sVersion
  val http4sCirce       = "org.http4s" %% "http4s-circe"        % http4sVersion
  val http4sDSL         = "org.http4s" %% "http4s-dsl"          % http4sVersion
  val http4sTwirl       = "org.http4s" %% "http4s-twirl"        % http4sVersion

  val monix = "io.monix" %% "monix" % "3.0.0-RC2"

  val scalatest           = "org.scalatest"              %% "scalatest"                 % "3.0.5" % Test
  val scalactic           = "org.scalactic"              %% "scalactic"                 % "3.0.5" % Test
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.13.5" % Test
  val scalacheckNoTest    = "org.scalacheck"             %% "scalacheck"                % "1.13.5"
  val scalacheckShapeless = "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.8" % Test

  val jwnl = "net.sf.jwordnet" % "jwnl" % "1.4_rc3"

  val jaxbImpl        = "com.sun.xml.bind" % "jaxb-impl"  % "2.2.11"
  val jaxbCore        = "com.sun.xml.bind" % "jaxb-core"  % "2.2.11"
  val javaxActivation = "javax.activation" % "activation" % "1.1.1"

  val stanfordNlp              = "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1"
  val stanfordNlpModelsEnglish = "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models"
  val stanfordNlpModelsGerman  = "edu.stanford.nlp" % "stanford-corenlp" % "3.9.1" classifier "models-german"

  val scopt = "com.github.scopt" %% "scopt" % "4.0.0-RC2"

  val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.0"
  val logbackClassic = "ch.qos.logback"             % "logback-classic" % "1.2.3"

  val scavenger = "org.aossie" %% "scavenger" % "0.2.1-SNAPSHOT"

  val scalaJsScripts = "com.vmunier" %% "scalajs-scripts" % "1.1.2"

  val webjarBootstrap = "org.webjars"     % "bootstrap" % "4.3.1"
  val webjarJquery    = "org.webjars"     % "jquery"    % "3.3.1-2"
  val webjarPopper    = "org.webjars.npm" % "popper.js" % "1.14.6"
  val webjarD3js      = "org.webjars"     % "d3js"      % "5.5.0"
  val webjarDagreD3   = "org.webjars.npm" % "dagre-d3"  % "0.6.3"

  val kindProjector    = compilerPlugin("org.spire-math" %% "kind-projector"     % "0.9.8")
  val betterMonadicFor = compilerPlugin("com.olegpy"     %% "better-monadic-for" % "0.3.0-M4")

  val circeDependencies =
    Def.setting[Seq[ModuleID]](
      Seq(
        "io.circe" %%% "circe-core"           % circeVersion,
        "io.circe" %%% "circe-generic"        % circeVersion,
        "io.circe" %%% "circe-generic-extras" % circeVersion,
        "io.circe" %%% "circe-literal"        % circeVersion,
        "io.circe" %%% "circe-parser"         % circeVersion
      )
    )
  val http4sDependencies: Seq[ModuleID] =
    Seq(
      http4sDSL,
      http4sBlazeServer,
      http4sBlazeClient,
      http4sCirce,
      http4sTwirl
    )

  val testingDependencies: Seq[ModuleID] =
    Seq(scalactic, scalatest, scalacheck)
  val loggingDependencies: Seq[ModuleID] =
    Seq(scalaLogging, logbackClassic)
  val compilerPlugins: Seq[ModuleID] =
    Seq(kindProjector, betterMonadicFor)
  val commonDependencies: Seq[ModuleID] =
    loggingDependencies ++ testingDependencies ++ compilerPlugins
}
