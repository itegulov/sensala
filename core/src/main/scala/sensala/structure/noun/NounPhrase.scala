package sensala.structure.noun

import sensala.property.Property
import sensala.structure.NL

trait NounPhrase extends NL {
  def properties: List[Property]
}
