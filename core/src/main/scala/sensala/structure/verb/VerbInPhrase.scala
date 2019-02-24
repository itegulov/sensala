package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.All
import sensala.error.NLInvalidState
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types._
import sensala.structure.prepositional.PrepositionalPhrase

case class VerbInPhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  propositionalPhrase: PrepositionalPhrase[F],
  verbPhrase: VerbPhrase[F]
) extends VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    verbPhrase.interpret(
      for {
        e <- LocalContext[F].getEvent
        locationL <- propositionalPhrase.nounPhrase.interpret(
                      for {
                        x <- LocalContext[F].getEntity
                        w = Sym(propositionalPhrase.word)
                        properties <- Context[F].eventProperties(e) >>= {
                                       case All(`e`, `event`, body) =>
                                         Monad[F].pure[E](body)
                                       case _ =>
                                         FunctorRaiseNLError[F].raise[E](
                                           NLInvalidState("Unexpected properties format")
                                         )
                                     }
                        _     <- Context[F].addEvent(e, properties /\ w(e, x))
                        contL <- cont
                      } yield w(e, x) /\ contL
                    )
      } yield locationL
    )
}
