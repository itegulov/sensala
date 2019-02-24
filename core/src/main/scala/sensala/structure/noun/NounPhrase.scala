package sensala.structure.noun

import sensala.property.Property
import sensala.structure._

trait NounPhrase[F[_]] extends NL[F] {
  def properties: List[Property]
  def definiteProperties: List[Property]
}
