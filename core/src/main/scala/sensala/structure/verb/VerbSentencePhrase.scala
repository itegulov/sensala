package sensala.structure.verb

import org.aossie.scavenger.expression.{Abs, E, i}
import sensala.structure.{CState, bindFreeVar}
import sensala.structure.noun.NounPhraseWithVerbPhrase

final case class VerbSentencePhrase(
  word: String,
  sentence: NounPhraseWithVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: E): CState = for {
    // TODO: probably I should use the verb somehow
    sentenceL <- sentence.interpret(cont)
    x <- bindFreeVar
  } yield Abs(x, i, sentenceL)
}