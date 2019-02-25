package sensala.structure.verb

import sensala.structure.prepositional.PrepositionalPhrase

final case class VerbInPhrase(
  propositionalPhrase: PrepositionalPhrase,
  verbPhrase: VerbPhrase
) extends VerbPhrase
