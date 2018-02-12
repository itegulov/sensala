package sensala.structure.verb

import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.structure.noun.NounPhraseWithVerbPhrase

final case class VerbSentencePhrase(
  word: String,
  sentence: NounPhraseWithVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      // TODO: probably I should use the verb somehow
      sentenceL <- sentence.interpret(cont)
    } yield sentenceL
}
