package sensala.error

import sensala.property.Property

sealed trait NLError

case class NLUnknownAnaphoricReferent(
  properties: List[Property]
) extends NLError

case class NLUnexpectedWord(
  word: String
) extends NLError