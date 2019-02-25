package sensala.structure.noun.pronoun

import sensala.property.Property

final case class DemonstrativePronoun(word: String) extends Pronoun {
  override def properties: List[Property]         = List.empty
  override def definiteProperties: List[Property] = List.empty
}
