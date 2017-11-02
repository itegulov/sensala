package sensala.structure

import org.aossie.scavenger.expression.E

trait NL {
  def interpret(cont: E): CState
}

trait Word extends NL {
  val word: String
}
