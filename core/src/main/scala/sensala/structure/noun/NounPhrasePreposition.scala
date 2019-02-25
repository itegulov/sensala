package sensala.structure.noun

import sensala.property.Property
import sensala.structure.prepositional.PrepositionalPhrase

final case class NounPhrasePreposition(
  prepositionalPhrase: PrepositionalPhrase,
  nounPhrase: NounPhrase
) extends NounPhrase {
  // TODO: Add preposition property
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
