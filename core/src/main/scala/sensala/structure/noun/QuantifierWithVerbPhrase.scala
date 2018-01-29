package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import org.atnos.eff.all._
import sensala.structure.verb.VerbPhrase

trait QuantifierWithVerbPhrase extends NounPhraseWithVerbPhrase

final case class ForallQuantifierVP(
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends QuantifierWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      _ <- modify[NLFx, Context](_.addReferent(x, properties))
      nounL <- nounPhrase.interpret(
                for {
                  verbL <- verbPhrase.interpret(cont)
                } yield Abs(y, i, ~verbL(y))
              )
      // TODO: understand the scope of forall quantifier
//      _ <- modify(_.deleteReferent(x))
    } yield All(x, i, Neg(App(nounL, x)))

  override def properties = nounPhrase.properties
}

final case class ExistentialQuantifierVP(
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends QuantifierWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addReferent(x, properties))
      nounL <- nounPhrase.interpret(verbPhrase.interpret(cont))
    } yield Ex(x, i, nounL(x))

  override def properties = nounPhrase.properties
}
