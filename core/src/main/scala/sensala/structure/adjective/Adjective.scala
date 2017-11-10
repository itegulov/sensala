package sensala.structure.adjective

import cats.data.State
import org.aossie.scavenger.expression.Sym
import sensala.structure.{CState, Word}

final case class Adjective(word: String) extends Word {
  override def interpret(cont: CState) = State.pure(Sym(word))
}
