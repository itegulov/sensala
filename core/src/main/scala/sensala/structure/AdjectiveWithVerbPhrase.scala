package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait AdjectiveWithVerbPhrase extends NounPhraseWithVerbPhrase

final case class AdjectivePhraseVP(adjective: Adjective, nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends AdjectiveWithVerbPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      nounL <- nounPhrase.interpret
      verbL <- verbPhrase.interpret
      w = Sym(adjective.word)
    } yield Abs(f, i -> o, Abs(x, i, AppRec(nounL, List(Abs(y, i, And(App(w, y), AppRec(verbL, List(f, y)))), x))))

  override def gender = nounPhrase.gender
}
