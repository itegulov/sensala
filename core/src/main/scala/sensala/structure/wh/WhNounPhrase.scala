package sensala.structure.wh

import org.aossie.scavenger.expression._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure.{CState, bindFreeVar}

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Abs(y, i, App(verbL, y)))
    } yield Abs(x, i, App(nounL, x))

  override def gender = nounPhrase.gender
}
