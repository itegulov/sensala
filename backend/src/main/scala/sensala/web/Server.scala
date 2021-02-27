package sensala.web

import cats.Monad
import cats.effect._
import distage.Injector
import izumi.distage.model.definition.ModuleDef
import izumi.distage.model.plan.GCMode
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.implicits._
import org.http4s.server.middleware._
import scopt.OParser
import sensala.error.NLError.FunctorRaiseNLError.raiseNLError
import sensala.shared.effect.Log
import sensala.interpreter.Interpreter
import sensala.interpreter.context.{Context, LocalContext}
import sensala.parser.english._
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.english.ParserError.HandleParserError.handleParserErrorIO
import sensala.property.{PropertyExtractor, WordNetPropertyExtractor}
import sensala.verbnet.VerbNetExtractor

import scala.concurrent.duration._

object Server extends IOApp {
  final case class Config(
    host: String = "localhost",
    port: Int = 8080
  )

  val builder = OParser.builder[Config]
  val parser = {
    import builder._
    OParser.sequence(
      programName("sensala"),
      head("sensala", "0.1"),
      opt[Int]('p', "port").action((port, c) => c.copy(port = port)),
      opt[String]('h', "host").action((host, c) => c.copy(host = host))
    )
  }

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val log: Log[IO] = Log.log
    OParser.parse(parser, args, Config()) match {
      case Some(config) =>
        implicit val sensalaContext: Context[IO]           = Context.initial
        implicit val sensalaLocalContext: LocalContext[IO] = LocalContext.empty
        val module = new ModuleDef {
          make[PronounParser[IO]]
          make[NounPhraseParser[IO]]
          make[VerbPhraseParser[IO]]
          make[DiscourseParser[IO]]
          addImplicit[Monad[IO]]
          addImplicit[HandleParserError[IO]]
        }

        val plan     = Injector().plan(module, GCMode.NoGC)
        val resource = Injector().produce(plan)
        implicit val parser = resource.use { objects =>
          objects.get[DiscourseParser[IO]]
        }
        for {
          implicit0(wordNetExtractor: WordNetPropertyExtractor[IO]) <- WordNetPropertyExtractor
                                                                        .create[IO]()
          implicit0(verbNetExtractor: VerbNetExtractor[IO])   <- VerbNetExtractor.create[IO]()
          implicit0(propertyExtractor: PropertyExtractor[IO]) = PropertyExtractor[IO]()
          implicit0(interpreter: Interpreter[IO])             = Interpreter[IO]()
          applicationService                                  = ApplicationService[IO]()
          httpApp = Router[IO](
            "/" -> applicationService.application
          )
          methodConfig = CORSConfig(
            anyOrigin = true,
            anyMethod = false,
            allowedMethods = Some(Set("GET", "POST", "PUT")),
            allowCredentials = true,
            maxAge = 1.day.toSeconds
          )
          corsApp = CORS(httpApp, methodConfig).orNotFound
          serverBuilder = BlazeServerBuilder[IO]
            .bindHttp(config.port, config.host)
            .withHttpApp(corsApp)
          result <- serverBuilder.serve.compile.drain.as(ExitCode.Success)
        } yield result
      case None =>
        log.error("Invalid arguments").as(ExitCode.Error)
    }
  }
}
