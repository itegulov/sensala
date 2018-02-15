package sensala.structure.prepositional

import sensala.structure.Word
import sensala.structure.noun.NounPhrase

trait PrepositionalPhrase extends Word {
  val nounPhrase: NounPhrase
}

case class InPhrase(word: String, nounPhrase: NounPhrase) extends PrepositionalPhrase
