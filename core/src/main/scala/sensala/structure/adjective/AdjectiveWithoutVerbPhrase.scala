package sensala.structure.adjective

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import org.atnos.eff._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure._

trait AdjectiveWithoutVerbPhrase extends AdjectivePhrase with NounPhraseWithoutVerbPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      contL <- cont
      nounL <- nounPhrase.interpret(Eff.pure(Abs(y, i, And(App(w, y), App(contL, y)))))
    } yield Abs(x, i, App(nounL, x))

  override def properties = nounPhrase.properties
}
