package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure._
import sensala.structure.noun.NounPhraseWithVerbPhrase
import sensala.structure.types.event

final case class VerbSentencePhrase(
  word: String,
  sentence: NounPhraseWithVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      e <- bindFreeVar
      x <- getEntity
      _ <- putEvent(e)
      sentenceL <- sentence.interpret(
                    for {
                      eSucc <- getEvent
                      w     = Sym(word)
                      contL <- cont
                    } yield w(e) /\: agent(e, x) /\: patient(e, eSucc) /\: contL
                  )
    } yield Ex(e, event, sentenceL)
}
