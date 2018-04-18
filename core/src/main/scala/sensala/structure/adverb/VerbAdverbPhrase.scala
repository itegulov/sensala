package sensala.structure.adverb

import org.aossie.scavenger.expression.formula.All
import org.aossie.scavenger.expression.{E, Sym}
import org.atnos.eff.all._
import sensala.error.{NLError, NLInvalidState}
import sensala.structure._
import sensala.structure.types.event
import sensala.structure.verb.VerbPhrase

case class VerbAdverbPhrase(adverb: Adverb, verbPhrase: VerbPhrase)
    extends AdverbPhrase
    with VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verbPhrase.interpret(
      for {
        e <- getEvent
        w = Sym(adverb.word)
        properties <- gets[NLFx, Context, E](_.eventProperties(e)) >>= {
                       case All(`e`, `event`, body) =>
                         pure(body)
                       case _ =>
                         left[NLFx, NLError, E](NLInvalidState("Unexpected properties format"))
                     }
        _     <- modify[NLFx, Context](_.addEvent(e, properties /\ w(e)))
        contL <- cont
      } yield w(e) /\ contL
    )
}
