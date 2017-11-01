package sensala.structure

import org.aossie.scavenger.expression._

case class WhNounPhrase(verbPhrase: VerbPhrase, nounPhrase: NounPhrase) extends NounPhraseWithoutVerbPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      nounL <- nounPhrase.interpret
      verbL <- verbPhrase.interpret
    } yield Abs(f, i -> o, Abs(x, i, AppRec(nounL, List(Abs(y, i, App(App(verbL, f), y)), x))))

  override def gender = nounPhrase.gender
}
