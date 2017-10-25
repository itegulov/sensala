package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

import contextMonad._

trait NounPhrase extends NL

case class ProperNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeSym
      q <- bindFreeSym
      f <- bindFreeSym
      w = Sym(word)
      _ <- modify(_.addReferent(x))
    } yield Abs(q, i, Abs(f, i, And(App(w, x), App(App(q, x), f))))
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
    ref <- inspect(_.findAnaphoricReferent.get)
  } yield Abs(p, i, Abs(f, i, App(App(p, ref), f)))
}
