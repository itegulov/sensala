package sensala.error

import sensala.property.Property

sealed trait NLError

final case class NLUnknownAnaphoricReferent(
  properties: List[Property]
) extends NLError

final case class NLUnexpectedWord(
  word: String
) extends NLError

final case class NLInvalidState(
  error: String
) extends NLError