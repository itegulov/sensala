package sensala.structure.adjective

import org.aossie.scavenger.expression._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure._

trait AdjectiveWithoutVerbPhrase extends AdjectivePhrase with NounPhraseWithoutVerbPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret(
      for {
        y     <- getEntity
        w     = Sym(adjective.word)
        contL <- cont
      } yield w(y) /\: contL
    )

  override def properties = nounPhrase.properties
}
