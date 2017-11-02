package sensala.structure.adjective

import cats.data.State
import org.aossie.scavenger.expression.{E, Sym}
import sensala.structure.Word

final case class Adjective(word: String) extends Word {
  override def interpret(cont: E) = State.pure(Sym(word))
}
