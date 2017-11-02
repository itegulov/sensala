package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

import contextMonad._

trait NounPhraseWithoutVerbPhrase extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, And(App(w, x), App(cont, x)))

  override def gender: Gender = word match {
    case "Mary" => Female
    case "John" => Male
    case _      => Other
  }
}

case class CommonNoun(
  word: String
) extends Word with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, And(App(w, x), App(cont, x)))

  override def gender: Gender = word match {
    case "farmer" => Male
    case "lawyer" => Male
    case "donkey" => Other
    case _        => Other
  }
}

final case class ReflexivePronoun(
  word: String
) extends Word with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              inspect(_.findAnaphoricReferent(Abs(x, i, App(nonHuman, x))).get)
            else if (word.toLowerCase == "he")
              inspect(_.findAnaphoricReferent(Abs(x, i, App(male, x))).get)
            else ???
    } yield App(cont, ref)

  override def gender: Gender = word match {
    case "he"  => Male
    case "she" => Female
    case "it"  => Other
    case _     => Other
  }
}
