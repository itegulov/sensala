package sensala.web

import cats.effect.{ContextShift, Effect, Sync}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.staticcontent.WebjarService.Config
import org.http4s.server.staticcontent.webjarService

final case class WebjarService[F[_]: Sync: Effect: ContextShift]() extends Http4sDsl[F] {
  val webjars: HttpRoutes[F] = webjarService(
    Config(
      blockingExecutionContext = scala.concurrent.ExecutionContext.global
    )
  )
}
