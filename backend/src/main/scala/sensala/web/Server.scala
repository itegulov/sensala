package sensala.web

import cats.Functor
import cats.effect._
import cats.implicits._
import cats.mtl.FunctorRaise
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s.implicits._
import sensala.effect.Log
import sensala.error.NLError
import sensala.error.NLError.FunctorRaiseNLError
import sensala.interpreter.Interpreter
import sensala.interpreter.context.{Context, LocalContext}
import sensala.property.PropertyExtractor

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val log: Log[IO] = Log.log
    implicit val raiseNLError: FunctorRaiseNLError[IO] = new FunctorRaise[IO, NLError] {
      override val functor: Functor[IO] = Functor[IO]

      override def raise[A](e: NLError): IO[A] =
        throw new RuntimeException(e.toString)
    }
    implicit val sensalaContext: Context[IO]              = Context.initial
    implicit val sensalaLocalContext: LocalContext[IO]    = LocalContext.empty
    implicit val propertyExtractor: PropertyExtractor[IO] = PropertyExtractor()
    implicit val interpreter: Interpreter[IO]             = Interpreter()
    val applicationService                                = ApplicationService[IO]()
    val webjarService                                     = WebjarService[IO]()
    val staticFileService                                 = StaticFileService[IO]()
    val httpApp = Router[IO](
      "/assets" -> staticFileService.staticFiles,
      "/assets" -> webjarService.webjars,
      "/"       -> applicationService.application
    ).orNotFound
    val serverBuilder = BlazeServerBuilder[IO].bindHttp(8080, "localhost").withHttpApp(httpApp)
    serverBuilder.serve.compile.drain.as(ExitCode.Success)
  }
}
