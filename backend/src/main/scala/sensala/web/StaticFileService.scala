package sensala.web

import cats.effect.{ContextShift, Sync}
import org.http4s.{HttpRoutes, StaticFile}
import org.http4s.dsl.Http4sDsl

import scala.concurrent.ExecutionContext.global

final case class StaticFileService[F[_]: Sync: ContextShift]()(implicit http4sDsl: Http4sDsl[F]) {
  import http4sDsl._

  val staticFiles = HttpRoutes.of[F] {
    // Serve static css files
    case request @ GET -> Root / "css" / path if path.endsWith(".css") =>
      StaticFile
        .fromResource("/css/" + path, global, Some(request))
        .getOrElseF(NotFound())
    // Serve ScalaJS generated resources
    case request @ GET -> Root / path if path.endsWith(".js") =>
      StaticFile
        .fromResource("/" + path, global, Some(request))
        .getOrElseF(NotFound())
  }
}
