package sensala.error

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
    def apply[F[_]](implicit ev: FunctorRaiseNLError[F]): FunctorRaiseNLError[F] = ev
  }
}
