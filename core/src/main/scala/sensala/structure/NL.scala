package sensala.structure

import cats.data.State
import org.aossie.scavenger.expression.{E, Sym}

trait NL {
  def interpret(cont: E): CState
}

trait NounPhrase extends NL {
  def gender: Gender
}

final case class Adjective(word: String) extends Word {
  override def interpret(cont: E) = State.pure(Sym(word))
}

trait Word extends NL {
  val word: String
}
