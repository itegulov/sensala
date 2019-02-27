package sensala.web

import cats.Functor
import cats.effect._
import cats.implicits._
import cats.mtl.FunctorRaise
import monix.eval.Task
import org.http4s.dsl.Http4sDsl
import org.http4s.server.Router
import org.http4s.server.blaze._
import org.http4s._
import org.http4s.implicits._
import sensala.error.NLError
import sensala.interpreter.Interpreter
import sensala.interpreter.context.{Context, LocalContext}
import sensala.property.PropertyExtractor

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    implicit val raiseNLError = new FunctorRaise[IO, NLError] {
      override val functor: Functor[IO] = Functor[IO]

      override def raise[A](e: NLError): IO[A] =
        throw new RuntimeException(e.toString)
    }
    implicit val sensalaContext: Context[IO]           = Context.initial[IO]
    implicit val sensalaLocalContext: LocalContext[IO] = LocalContext.empty[IO]
    implicit val propertyExtractor                     = PropertyExtractor[IO]()
    implicit val interpreter: Interpreter[IO]          = Interpreter[IO]()
    implicit val http4sDsl: Http4sDsl[IO]              = Http4sDsl[IO]
    val applicationService                             = ApplicationService[IO]()
    val webjarService                                  = WebjarService[IO]()
    val staticFileService                              = StaticFileService[IO]()
    val httpApp = Router[IO](
      "/assets" -> staticFileService.staticFiles,
      "/assets" -> webjarService.webjars,
      "/"       -> applicationService.application
    ).orNotFound
    val serverBuilder = BlazeServerBuilder[IO].bindHttp(8080, "localhost").withHttpApp(httpApp)
    serverBuilder.serve.compile.drain.as(ExitCode.Success)
  }
}
