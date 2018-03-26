package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.All
import org.atnos.eff.all._
import sensala.error.{NLError, NLInvalidState}
import sensala.structure._
import sensala.structure.types._
import sensala.structure.prepositional.PrepositionalPhrase

case class VerbInPhrase(propositionalPhrase: PrepositionalPhrase, verbPhrase: VerbPhrase)
    extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verbPhrase.interpret(
      for {
        e <- getEvent
        locationL <- propositionalPhrase.nounPhrase.interpret(
                      for {
                        x <- getEntity
                        w = Sym(propositionalPhrase.word)
                        properties <- gets[NLFx, Context, E](_.eventProperties(e)) >>= {
                                       case All(`e`, `event`, body) =>
                                         pure(body)
                                       case _ =>
                                         left[NLFx, NLError, E](
                                           NLInvalidState("Unexpected properties format")
                                         )
                                     }
                        _     <- modify[NLFx, Context](_.addEvent(e, properties /\: w(e, x)))
                        contL <- cont
                      } yield w(e, x) /\: contL
                    )
      } yield locationL
    )
}
