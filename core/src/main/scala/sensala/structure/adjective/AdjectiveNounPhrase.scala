package sensala.structure.adjective

import org.aossie.scavenger.expression._
import sensala.structure.noun.NounPhrase
import sensala.structure._

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectivePhrase with NounPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret(
      for {
        y     <- getEntity
        w     = Sym(adjective.word)
        contL <- cont
      } yield w(y) /\ contL
    )

  override def properties = nounPhrase.properties
}
