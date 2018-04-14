package sensala.property

import org.aossie.scavenger.expression.{E, Sym}

final case class Property(propertyExp: Sym => E) extends AnyVal
