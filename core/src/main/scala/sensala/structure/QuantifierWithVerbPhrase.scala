package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._

import contextMonad._

trait QuantifierWithVerbPhrase extends NounPhraseWithVerbPhrase

case class ForallQuantifierVP(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends QuantifierWithVerbPhrase {
  override def interpret(cont: E): CState = for {
    x <- bindFreeVar
    y <- bindFreeVar
    z <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
    nounL <- nounPhrase.interpret(z)
    verbL <- verbPhrase.interpret(cont)
    // TODO: understand the scope of forall quantifier
//    _ <- modify(_.deleteReferent(x))
  } yield All(x, i, Neg(App(App(Abs(z, i, nounL), Abs(y, i, Neg(App(verbL, y)))), x)))

  override def gender = nounPhrase.gender
}

case class ExistentialQuantifierVP(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends QuantifierWithVerbPhrase {
  override def interpret(cont: E): CState = for {
    x <- bindFreeVar
    y <- bindFreeVar
    _ <- modify(_.addReferent(x, gender))
    nounL <- nounPhrase.interpret(y)
    verbL <- verbPhrase.interpret(cont)
  } yield Ex(x, i, App(App(Abs(y, i, nounL), verbL), x))

  override def gender = nounPhrase.gender
}

