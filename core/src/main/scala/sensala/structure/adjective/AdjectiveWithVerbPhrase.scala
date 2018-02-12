package sensala.structure.adjective

import org.aossie.scavenger.expression._
import sensala.structure.noun.{NounPhrase, NounPhraseWithVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._

trait AdjectiveWithVerbPhrase extends AdjectivePhrase with NounPhraseWithVerbPhrase

final case class AdjectiveNounPhraseVP(
  adjective: Adjective,
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends AdjectiveWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret(
      for {
        y <- getEntity
        w = Sym(adjective.word)
        verbL <- verbPhrase.interpret(cont)
      } yield w(y) /\: verbL
    )

  override def properties = nounPhrase.properties
}
