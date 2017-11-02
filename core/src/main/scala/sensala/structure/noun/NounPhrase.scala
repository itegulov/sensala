package sensala.structure.noun

import sensala.structure.{Gender, NL}

trait NounPhrase extends NL {
  def gender: Gender
}
