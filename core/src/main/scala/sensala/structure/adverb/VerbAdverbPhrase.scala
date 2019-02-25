package sensala.structure.adverb

import sensala.structure.verb.VerbPhrase

final case class VerbAdverbPhrase(
  adverb: Adverb,
  verbPhrase: VerbPhrase
) extends AdverbPhrase
    with VerbPhrase
