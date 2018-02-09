package sensala.structure

import org.aossie.scavenger.expression.Var

case class LocalContext(entity: Option[Var], event: Option[Var])

object LocalContext {
  def empty: LocalContext = LocalContext(None, None)
}
