package sensala.structure

import org.aossie.scavenger.expression._

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Abs(y, i, App(verbL, y)))
    } yield Abs(x, i, App(nounL, x))

  override def gender = nounPhrase.gender
}
