package sensala.structure.adjective

import sensala.structure.NL

trait AdjectivePhrase[F[_]] extends NL[F] {
  val adjective: Adjective
}
