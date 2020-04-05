package sensala.parser.english

import cats.mtl.ApplicativeHandle

sealed trait ParserError extends Exception

final case class InvalidDiscourse(message: String) extends ParserError

object ParserError {
  type HandleParserError[F[_]] = ApplicativeHandle[F, ParserError]

  object HandleParserError {
    def apply[F[_]](implicit ev: HandleParserError[F]): HandleParserError[F] = ev
  }
}
