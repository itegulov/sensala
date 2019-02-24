package sensala.structure.adverb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.formula.All
import org.aossie.scavenger.expression.{E, Sym}
import sensala.error.NLInvalidState
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types.event
import sensala.structure.verb.VerbPhrase

case class VerbAdverbPhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  adverb: Adverb,
  verbPhrase: VerbPhrase[F]
) extends AdverbPhrase[F]
    with VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    verbPhrase.interpret(
      for {
        e <- LocalContext[F].getEvent
        w = Sym(adverb.word)
        properties <- Context[F].eventProperties(e) >>= {
                       case All(`e`, `event`, body) =>
                         Monad[F].pure[E](body)
                       case _ =>
                         FunctorRaiseNLError[F].raise[E](NLInvalidState("Unexpected properties format"))
                     }
        _     <- Context[F].addEvent(e, properties /\ w(e))
        contL <- cont
      } yield w(e) /\ contL
    )
}
