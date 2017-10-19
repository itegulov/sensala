package au.edu.anu.sensala.structure

import cats.data.State

trait NounPhrase extends NL

case class ProperNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    State { s =>
      val sym = Sym(word)
      (s.extend(sym), sym)
    }
}
case class CommonNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    State.pure {
      val w = Sym(word)
      val x = Sym("x")
      Abs(x, App(w, x))
    }
}

case class ReflexivePronoun(word: String) extends Word with NounPhrase {
  override def interpret: CState = State.inspect(_.findAnaphoricReferent.get)
}
