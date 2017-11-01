package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._

import contextMonad._

trait QuantifierWithoutVerbPhrase extends NounPhraseWithoutVerbPhrase

case class ForallQuantifier(nounPhrase: NounPhrase) extends QuantifierWithoutVerbPhrase {
  override def interpret: CState = for {
    f <- bindFreeVar
    nounL <- nounPhrase.interpret
    x <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
    // TODO: understand the scope of forall quantifier
//    _ <- modify(_.deleteReferent(x))
  } yield Abs(f, i -> o, All(x, i, And(Neg(App(App(nounL, True), x)), App(f, x))))

  override def gender = nounPhrase.gender
}

case class ExistentialQuantifier(nounPhrase: NounPhrase) extends QuantifierWithoutVerbPhrase {
  override def interpret: CState = for {
    f <- bindFreeVar
    p <- nounPhrase.interpret
    x <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
  } yield Abs(f, i -> o, Ex(x, i, AppRec(p, List(f, x))))

  override def gender = nounPhrase.gender
}

