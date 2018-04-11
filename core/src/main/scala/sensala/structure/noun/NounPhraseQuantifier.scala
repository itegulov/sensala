package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import org.atnos.eff._
import org.atnos.eff.all._
import sensala.structure.types._

sealed trait NounPhraseQuantifier extends NounPhrase

final case class ForallQuantifier(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      nounL <- nounPhrase.interpret(cont)
      x     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addReferent(x, properties))
      _     <- putEntity(x)
      // TODO: understand the scope of forall quantifier
//      _ <- modify(_.deleteReferent(x))=
    } yield All(x, entity, ~nounL)

  override def properties = nounPhrase.properties
}

final case class ExistentialQuantifier(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addReferent(x, properties))
      _     <- putEntity(x)
      nounL <- nounPhrase.interpret(cont)
    } yield Ex(x, entity, nounL)

  override def properties = nounPhrase.properties
}