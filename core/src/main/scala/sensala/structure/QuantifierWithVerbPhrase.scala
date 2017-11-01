package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._

import contextMonad._

trait QuantifierWithVerbPhrase extends NounPhraseWithVerbPhrase

case class ForallQuantifierVP(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends QuantifierWithVerbPhrase {
  override def interpret: CState = for {
    f <- bindFreeVar
    nounL <- nounPhrase.interpret
    x <- bindFreeVar
    y <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
    verbL <- verbPhrase.interpret
    // TODO: understand the scope of forall quantifier
//    _ <- modify(_.deleteReferent(x))
  } yield Abs(f, i -> o, All(x, i, Neg(AppRec(nounL, List(Abs(y, i, Neg(AppRec(verbL, List(f, y)))), x)))))

  override def gender = nounPhrase.gender
}

case class ExistentialQuantifierVP(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends QuantifierWithVerbPhrase {
  override def interpret: CState = for {
    f <- bindFreeVar
    nounL <- nounPhrase.interpret
    x <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
    verb <- verbPhrase.interpret
  } yield Abs(f, i, Ex(x, i, AppRec(nounL, List(App(verb, f), x))))

  override def gender = nounPhrase.gender
}

