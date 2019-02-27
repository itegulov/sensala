package sensala.web

import cats.effect.{ContextShift, Sync}
import org.http4s.{HttpRoutes, StaticFile}
import org.http4s.dsl.Http4sDsl

import scala.concurrent.ExecutionContext.global

final case class StaticFileService[F[_]: Sync: ContextShift]() extends Http4sDsl[F] {
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
