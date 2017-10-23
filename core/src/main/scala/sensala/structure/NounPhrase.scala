package sensala.structure

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait NounPhrase extends NL

case class ProperNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeSym
      w = Sym(word)
      _ <- State.modify[Context](_.extend(w))
    } yield Abs(x, i, App(x, w))
}

case class CommonNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeSym
      f <- bindFreeSym
      w = Sym(word)
    } yield Abs(x, i, Abs(f, i, And(App(w, x), f)))
}

case class ReflexivePronoun(word: String) extends Word with NounPhrase {
  override def interpret: CState = for {
    p <- bindFreeSym
    f <- bindFreeSym
    ref <- State.inspect[Context, Sym](_.findAnaphoricReferent.get)
  } yield Abs(p, i, Abs(f, i, App(App(p, ref), f)))
}
