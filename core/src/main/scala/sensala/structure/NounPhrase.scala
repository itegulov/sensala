package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

import contextMonad._

trait NounPhrase extends NL {
  def gender: Gender
}

case class ProperNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeVar
      f <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, Abs(f, i, And(App(w, x), f)))

  override def gender: Gender = word match {
    case "Mary" => Female
    case "John" => Male
    case _ => Other
  }
}

case class CommonNoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeVar
      f <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, Abs(f, i, And(App(w, x), f)))

  override def gender: Gender = word match {
    case "farmer" => Male
    case "donkey" => Other
    case _ => Other
  }
}

case class ReflexivePronoun(word: String) extends Word with NounPhrase {
  override def interpret: CState =
    for {
      p <- bindFreeVar
      f <- bindFreeVar
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it") inspect(_.findAnaphoricReferent(Abs(x, i, App(nonHuman, x))).get)
            else if (word.toLowerCase == "he") inspect(_.findAnaphoricReferent(Abs(x, i, App(male, x))).get)
            else ???
    } yield Abs(p, i, Abs(f, i, App(App(p, ref), f)))

  override def gender: Gender = word match {
    case "he" => Male
    case "she" => Female
    case "it" => Other
    case _ => Other
  }
}
