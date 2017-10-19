package au.edu.anu.sensala.structure

import cats.data.State

trait NL {
  def interpret: CState
}

trait Word extends NL {
  val word: String
  override def interpret: CState = State.pure(Sym(word))
}
