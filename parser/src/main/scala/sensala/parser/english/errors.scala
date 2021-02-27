package sensala.parser.english

import cats.{Applicative, Functor}
import cats.effect.IO
import cats.mtl.{ApplicativeHandle, DefaultApplicativeHandle}

sealed trait ParserError extends Exception {
  val message: String

  override def toString: String = s"Unable to parse: $message"
}

final case class InvalidDiscourse(message: String) extends ParserError
final case class UnexpectedWord(word: String) extends ParserError {
  override val message: String = s"""Unexpected word "$word""""
}

object ParserError {
  type HandleParserError[F[_]] = ApplicativeHandle[F, ParserError]

  object HandleParserError {
    implicit val handleParserErrorIO: HandleParserError[IO] =
      new DefaultApplicativeHandle[IO, ParserError] {
        override val applicative: Applicative[IO] = Applicative[IO]

        override val functor: Functor[IO] = Functor[IO]

        override def handleWith[A](fa: IO[A])(f: ParserError => IO[A]): IO[A] =
          fa.handleErrorWith {
            case e: ParserError => f(e)
            case t: Throwable   => throw t
          }

        override def raise[A](e: ParserError): IO[A] = IO.raiseError(e)
      }

    def apply[F[_]](implicit ev: HandleParserError[F]): HandleParserError[F] = ev
  }
}
