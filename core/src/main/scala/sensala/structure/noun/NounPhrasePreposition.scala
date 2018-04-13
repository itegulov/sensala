package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.property.Property
import sensala.structure._
import sensala.structure.prepositional.PrepositionalPhrase

final case class NounPhrasePreposition(
  prepositionalPhrase: PrepositionalPhrase,
  nounPhrase: NounPhrase
) extends NounPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret {
      for {
        e <- getEntity
        preposition <- prepositionalPhrase.nounPhrase.interpret {
          for {
            prepEntity <- getEntity
            w = Sym(prepositionalPhrase.word)
            contL <- cont
          } yield w(prepEntity, e) /\: contL
        }
      } yield preposition
    }
  
  // TODO: Add preposition property
  override def properties: List[Property] = nounPhrase.properties
}
