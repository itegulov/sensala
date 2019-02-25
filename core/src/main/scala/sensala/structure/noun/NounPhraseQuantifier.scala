package sensala.structure.noun

import sensala.property.Property

sealed trait NounPhraseQuantifier extends NounPhrase

final case class ForallQuantifier(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class ExistentialQuantifier(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class DefiniteNounPhrase(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
