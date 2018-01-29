package sensala.structure

import org.aossie.scavenger.expression.E

trait NL {
  def interpret(cont: NLEff[E]): NLEff[E]
}

trait Word extends NL {
  val word: String
}
