package sensala.structure.verb

import sensala.structure.adjective.Adjective

final case class VerbAdjectivePhrase(verb: String, adjective: Adjective) extends VerbPhrase
