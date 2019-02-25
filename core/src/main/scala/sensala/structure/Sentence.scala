package sensala.structure

import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase

final case class Sentence(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NL
