package sensala.structure.propositional

import sensala.structure.Word
import sensala.structure.noun.NounPhrase

trait PropositionalPhrase extends Word {
  val nounPhrase: NounPhrase
}

case class In(word: String, nounPhrase: NounPhrase) extends PropositionalPhrase
