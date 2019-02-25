package sensala.structure.verb

import sensala.structure.noun.NounPhrase

final case class TransitiveVerb(word: String, obj: NounPhrase) extends VerbPhrase
