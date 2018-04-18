package sensala.structure.noun

import sensala.property.Property
import sensala.structure._

trait NounPhrase extends NL {
  def properties: List[Property]
  def definiteProperties: List[Property]
}
