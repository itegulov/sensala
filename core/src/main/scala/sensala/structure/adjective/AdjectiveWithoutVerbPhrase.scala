package sensala.structure.adjective

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure._

trait AdjectiveWithoutVerbPhrase extends AdjectivePhrase with NounPhraseWithoutVerbPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      nounL <- nounPhrase.interpret(Abs(y, i, And(App(w, y), App(cont, y))))
    } yield Abs(x, i, App(nounL, x))

  override def gender = nounPhrase.gender
}
