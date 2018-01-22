package sensala.structure.noun

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import org.atnos.eff._
import org.atnos.eff.all._

trait QuantifierWithoutVerbPhrase extends NounPhraseWithoutVerbPhrase

final case class ForallQuantifier(
  nounPhrase: NounPhrase
) extends QuantifierWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      nounL <- nounPhrase.interpret(Eff.pure(True))
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addReferent(x, properties))
      // TODO: understand the scope of forall quantifier
      //    _ <- modify(_.deleteReferent(x))
      contL <- cont
    } yield All(x, i, And(Neg(App(nounL, x)), App(contL, x)))

  override def properties = nounPhrase.properties
}

final case class ExistentialQuantifier(
  nounPhrase: NounPhrase
) extends QuantifierWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addReferent(x, properties))
      nounL <- nounPhrase.interpret(cont)
    } yield Ex(x, i, App(nounL, x))

  override def properties = nounPhrase.properties
}

