package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import contextMonad._

trait QuantifierWithoutVerbPhrase extends NounPhraseWithoutVerbPhrase

final case class ForallQuantifier(
  nounPhrase: NounPhrase
) extends QuantifierWithoutVerbPhrase {
  override def interpret(cont: E): CState = for {
    nounL <- nounPhrase.interpret(True)
    x <- bindFreeVar
    _ <- modify(_.addReferent(x, properties))
    // TODO: understand the scope of forall quantifier
//    _ <- modify(_.deleteReferent(x))
  } yield All(x, i, And(Neg(App(nounL, x)), App(cont, x)))

  override def properties = nounPhrase.properties
}

final case class ExistentialQuantifier(
  nounPhrase: NounPhrase
) extends QuantifierWithoutVerbPhrase {
  override def interpret(cont: E): CState = for {
    nounL <- nounPhrase.interpret(cont)
    x <- bindFreeVar
    _ <- modify(_.addReferent(x, properties))
  } yield Ex(x, i, App(nounL, x))

  override def properties = nounPhrase.properties
}
