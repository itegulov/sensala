package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait AdjectiveWithVerbPhrase extends NounPhraseWithVerbPhrase

final case class AdjectivePhraseVP(
  adjective: Adjective,
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends AdjectiveWithVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Abs(y, i, And(App(w, y), App(verbL, y))))
    } yield Abs(x, i, App(nounL, x))

  override def gender = nounPhrase.gender
}
