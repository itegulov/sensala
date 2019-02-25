package sensala.structure.adjective

import sensala.property.Property
import sensala.structure.noun.NounPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectivePhrase
    with NounPhrase {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
