package sensala.error

import org.aossie.scavenger.expression.E

sealed trait NLError

final case class NLUnknownAnaphoricReferent(
  properties: E
) extends NLError

final case class NLUnexpectedWord(
  word: String
) extends NLError

final case class NLInvalidState(
  error: String
) extends NLError
