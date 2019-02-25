package sensala.structure.prepositional

import sensala.structure.NL
import sensala.structure.noun.NounPhrase

sealed trait PrepositionalPhrase extends NL {
  val verbWord: String
  val nounPhrase: NounPhrase
}

final case class InPhrase(verbWord: String, nounPhrase: NounPhrase) extends PrepositionalPhrase

final case class PossessionPhrase(nounPhrase: NounPhrase) extends PrepositionalPhrase {
  val verbWord = "owns"
}
