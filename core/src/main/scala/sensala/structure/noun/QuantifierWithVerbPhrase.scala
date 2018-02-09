package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import org.atnos.eff.all._
import sensala.structure.types._
import sensala.structure.verb.VerbPhrase

trait QuantifierWithVerbPhrase extends NounPhraseWithVerbPhrase

final case class ForallQuantifierVP(
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends QuantifierWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      y     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addReferent(x, properties))
      _     <- putEntity(x)
      nounL <- nounPhrase.interpret(verbPhrase.interpret(cont).map(~_))
      // TODO: understand the scope of forall quantifier
//      _ <- modify(_.deleteReferent(x))
    } yield All(x, entity, ~nounL)

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
      _     <- putEntity(x)
      nounL <- nounPhrase.interpret(verbPhrase.interpret(cont))
    } yield Ex(x, entity, nounL)

  override def properties = nounPhrase.properties
}
