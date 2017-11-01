package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait AdjectiveWithoutVerbPhrase extends NounPhraseWithoutVerbPhrase

final case class AdjectivePhrase(adjective: Adjective, nounPhrase: NounPhrase) extends AdjectiveWithoutVerbPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      nounL <- nounPhrase.interpret
      w = Sym(adjective.word)
    } yield Abs(f, i -> o, Abs(x, i, AppRec(nounL, List(Abs(y, i, And(App(w, y), App(f, y))), x))))

  override def gender = nounPhrase.gender
}
