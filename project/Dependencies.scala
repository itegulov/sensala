import sbt._

object Dependencies {
  val catsVersion        = "2.1.1"
  val catsMtlVersion     = "0.7.1"
  val circeVersion       = "0.13.0"
  val distageVersion     = "0.10.3-M2"
  val http4sVersion      = "0.21.3"
  val scalatestVersion   = "3.1.1"
  val stanfordNlpVersion = "3.9.2"

  val catsCore            = "org.typelevel" %% "cats-core"     % catsVersion
  val catsLawsTest        = "org.typelevel" %% "cats-laws"     % catsVersion % Test
  val catsLawsTestkitTest = "org.typelevel" %% "cats-testkit"  % catsVersion % Test
  val catsEffect          = "org.typelevel" %% "cats-effect"   % catsVersion
  val catsMtl             = "org.typelevel" %% "cats-mtl-core" % catsMtlVersion
  val catsMtlLawsTest     = "org.typelevel" %% "cats-mtl-laws" % catsMtlVersion % Test

  val distageCore    = "io.7mind.izumi" %% "distage-core"              % distageVersion
  val distageConfig  = "io.7mind.izumi" %% "distage-extension-config"  % distageVersion
  val distageRoles   = "io.7mind.izumi" %% "distage-framework"         % distageVersion
  val distageDocker  = "io.7mind.izumi" %% "distage-framework-docker"  % distageVersion
  val distageTestkit = "io.7mind.izumi" %% "distage-testkit-scalatest" % distageVersion

  val http4sBlazeClient = "org.http4s" %% "http4s-blaze-client" % http4sVersion
  val http4sBlazeServer = "org.http4s" %% "http4s-blaze-server" % http4sVersion
  val http4sCirce       = "org.http4s" %% "http4s-circe"        % http4sVersion
  val http4sDSL         = "org.http4s" %% "http4s-dsl"          % http4sVersion
  val http4sTwirl       = "org.http4s" %% "http4s-twirl"        % http4sVersion

  val monix = "io.monix" %% "monix" % "3.3.0"

  val scalatest           = "org.scalatest"              %% "scalatest"                 % scalatestVersion % Test
  val scalactic           = "org.scalactic"              %% "scalactic"                 % scalatestVersion % Test
  val scalacheck          = "org.scalacheck"             %% "scalacheck"                % "1.14.1" % Test

  val extjwnl         = "net.sf.extjwnl" % "extjwnl"           % "2.0.2"
  val extjwnlDataWn31 = "net.sf.extjwnl" % "extjwnl-data-wn31" % "1.2"
  
  val jverbnet = "edu.mit" % "jverbnet" % "1.2.0.1"

  val jaxbImpl        = "com.sun.xml.bind" % "jaxb-impl"  % "2.2.11"
  val jaxbCore        = "com.sun.xml.bind" % "jaxb-core"  % "2.2.11"
  val javaxActivation = "javax.activation" % "activation" % "1.1.1"

  val stanfordNlp              = "edu.stanford.nlp" % "stanford-corenlp" % stanfordNlpVersion
  val stanfordNlpModelsEnglish = "edu.stanford.nlp" % "stanford-corenlp" % stanfordNlpVersion classifier "models"
  val stanfordNlpModelsGerman  = "edu.stanford.nlp" % "stanford-corenlp" % stanfordNlpVersion classifier "models-german"

  val scopt = "com.github.scopt" %% "scopt" % "4.0.0"

  val scalaLogging   = "com.typesafe.scala-logging" %% "scala-logging"  % "3.9.2"
  val logbackClassic = "ch.qos.logback"             % "logback-classic" % "1.2.3"

  val scavenger = "org.aossie" %% "scavenger" % "0.2.2-SNAPSHOT"
  
  val sourcecode = "com.lihaoyi" %% "sourcecode" % "0.1.9"

  val kindProjector    = compilerPlugin("org.typelevel" %% "kind-projector"     % "0.11.3" cross CrossVersion.full)
  val betterMonadicFor = compilerPlugin("com.olegpy"    %% "better-monadic-for" % "0.3.1")

  val digestDependencies =
    Seq(
      distageCore,
      distageConfig,
      distageDocker,
      distageRoles,
      distageTestkit
    )
  val circeDependencies =
    Def.setting[Seq[ModuleID]](
      Seq(
        "io.circe" %% "circe-core"           % circeVersion,
        "io.circe" %% "circe-generic"        % circeVersion,
        "io.circe" %% "circe-literal"        % circeVersion,
        "io.circe" %% "circe-parser"         % circeVersion
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
    loggingDependencies ++
      testingDependencies ++
      compilerPlugins ++
      digestDependencies :+
      sourcecode
}
