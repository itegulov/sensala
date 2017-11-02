package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import contextMonad._
import sensala.structure.verb.VerbPhrase

trait QuantifierWithVerbPhrase extends NounPhraseWithVerbPhrase

final case class ForallQuantifierVP(
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends QuantifierWithVerbPhrase {
  override def interpret(cont: E): CState = for {
    x <- bindFreeVar
    y <- bindFreeVar
    z <- bindFreeVar
    _ <- modify(_.addReferent(x, properties))
    nounL <- nounPhrase.interpret(z)
    verbL <- verbPhrase.interpret(cont)
    // TODO: understand the scope of forall quantifier
//    _ <- modify(_.deleteReferent(x))
  } yield All(x, i, Neg(App(App(Abs(z, i, nounL), Abs(y, i, Neg(App(verbL, y)))), x)))

  override def properties = nounPhrase.properties
}

final case class ExistentialQuantifierVP(
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends QuantifierWithVerbPhrase {
  override def interpret(cont: E): CState = for {
    x <- bindFreeVar
    y <- bindFreeVar
    _ <- modify(_.addReferent(x, properties))
    nounL <- nounPhrase.interpret(y)
    verbL <- verbPhrase.interpret(cont)
  } yield Ex(x, i, App(App(Abs(y, i, nounL), verbL), x))

  override def properties = nounPhrase.properties
}

