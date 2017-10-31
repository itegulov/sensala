package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

final case class Adjective(word: String) extends Word {
  override def interpret: CState =
    for {
      p <- bindFreeVar
      x <- bindFreeVar
      f <- bindFreeVar
      w = Sym(word)
    } yield Abs(p, i, Abs(x, i, Abs(f, i, App(App(p, x), And(App(w, x), f)))))
}

final case class AdjectivePhrase(adjective: Adjective, nounPhrase: NounPhrase) extends NounPhrase {
  override def interpret: CState =
    for {
      adjL <- adjective.interpret
      nounL <- nounPhrase.interpret
    } yield App(adjL, nounL)

  override def gender = nounPhrase.gender
}
