package sensala

import org.aossie.scavenger.expression.{Atomic, T}

package object types {
  case object event  extends T with Atomic
  case object entity extends T with Atomic
}
