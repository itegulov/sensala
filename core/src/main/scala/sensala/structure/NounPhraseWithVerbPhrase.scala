package sensala.structure

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import contextMonad._

trait NounPhraseWithVerbPhrase extends NounPhrase {
  val verbPhrase: VerbPhrase
}

case class ProperNounVP(word: String, verbPhrase: VerbPhrase) extends Word with NounPhraseWithVerbPhrase {
  override def interpret: CState =
    for {
      x <- bindFreeVar
      f <- bindFreeVar
      w = Sym(word)
      verb <- verbPhrase.interpret
    } yield Abs(f, i -> o, Abs(x, i, And(App(w, x), AppRec(verb, List(f, x)))))

  override def gender: Gender = word match {
    case "Mary" => Female
    case "John" => Male
    case _ => Other
  }
}

case class ReflexivePronounVP(word: String, verbPhrase: VerbPhrase) extends Word with NounPhraseWithVerbPhrase {
  override def interpret: CState =
    for {
      p <- bindFreeVar
      f <- bindFreeVar
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it") inspect(_.findAnaphoricReferent(Abs(x, i, App(nonHuman, x))).get)
            else if (word.toLowerCase == "he") inspect(_.findAnaphoricReferent(Abs(x, i, App(male, x))).get)
            else ???
      verb <- verbPhrase.interpret
    } yield Abs(f, i -> o, AppRec(verb, List(f, ref)))

  override def gender: Gender = word match {
    case "he" => Male
    case "she" => Female
    case "it" => Other
    case _ => Other
  }
}

