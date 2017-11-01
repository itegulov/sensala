package sensala.structure

import org.aossie.scavenger.expression.E

trait NL {
  def interpret(cont: E): CState
}

trait NounPhrase extends NL {
  def gender: Gender
}

final case class Adjective(word: String) extends Word {
  override def interpret(cont: E) = ???
}

trait Word extends NL {
  val word: String
}
