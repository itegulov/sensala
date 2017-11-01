package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait AdjectiveWithoutVerbPhrase extends NounPhraseWithoutVerbPhrase

final case class AdjectivePhrase(adjective: Adjective, nounPhrase: NounPhrase) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      nounL <- nounPhrase.interpret(Abs(y, i, And(App(w, y), App(cont, y))))
    } yield Abs(x, i, App(nounL, x))

  override def gender = nounPhrase.gender
}
