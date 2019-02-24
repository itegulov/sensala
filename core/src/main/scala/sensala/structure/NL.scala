package sensala.structure

import org.aossie.scavenger.expression.E

trait NL[F[_]] {
  def interpret(cont: F[E]): F[E]
}

trait Word[F[_]] extends NL[F] {
  val word: String
}
