package sensala.structure.adverb

import sensala.structure.NL

trait AdverbPhrase[F[_]] extends NL[F] {
  val adverb: Adverb
}
