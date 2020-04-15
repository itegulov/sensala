package sensala.error

import cats.Functor
import cats.effect.IO
import cats.mtl.FunctorRaise
import org.aossie.scavenger.expression.E

sealed trait NLError

final case class NLUnknownAnaphoricReferent(
  properties: E
) extends NLError

final case class NLUnexpectedWord(
  word: String
) extends NLError

final case class NLInvalidState(
  error: String
) extends NLError

object NLError {
  type FunctorRaiseNLError[F[_]] = FunctorRaise[F, NLError]

  object FunctorRaiseNLError {
    implicit val raiseNLError: FunctorRaiseNLError[IO] = new FunctorRaise[IO, NLError] {
      override val functor: Functor[IO] = Functor[IO]

      override def raise[A](e: NLError): IO[A] =
        throw new RuntimeException(e.toString)
    }

    def apply[F[_]](implicit ev: FunctorRaiseNLError[F]): FunctorRaiseNLError[F] = ev
  }
}
