package sensala.structure.verb

import org.aossie.scavenger.expression.formula.All
import org.aossie.scavenger.expression.{E, Sym}
import org.atnos.eff.all._
import sensala.error.{NLError, NLInvalidState}
import sensala.structure.adverb.AdverbPhrase
import sensala.structure._
import sensala.structure.types.event

case class VerbAdverbPhrase(adverb: String, verbPhrase: VerbPhrase)
    extends AdverbPhrase
    with VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verbPhrase.interpret(
      for {
        e <- getEvent
        w = Sym(adverb)
        properties <- gets[NLFx, Context, E](_.eventProperties(e)) >>= {
                       case All(`e`, `event`, body) =>
                         pure(body)
                       case _ =>
                         left[NLFx, NLError, E](NLInvalidState("Unexpected properties format"))
                     }
        _     <- modify[NLFx, Context](_.addEvent(e, properties /\: w(e)))
        contL <- cont
      } yield w(e) /\: contL
    )
}
