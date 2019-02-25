package sensala.structure.wh

import sensala.property.Property
import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhrase {
  override def properties                         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
