package sensala.structure.prepositional

import sensala.structure.Word
import sensala.structure.noun.NounPhrase

sealed trait PrepositionalPhrase extends Word {
  val nounPhrase: NounPhrase
}

final case class InPhrase(word: String, nounPhrase: NounPhrase) extends PrepositionalPhrase

final case class PossessionPhrase(nounPhrase: NounPhrase) extends PrepositionalPhrase {
  override val word: String = "owns"
}
