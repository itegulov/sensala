package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import sensala.structure._
import contextMonad._
import sensala.structure.verb.VerbPhrase

trait NounPhraseWithVerbPhrase extends NounPhrase {
  val verbPhrase: VerbPhrase
}

final case class ProperNounVP(
  word: String, verbPhrase: VerbPhrase
) extends Word with NounPhraseWithVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
      verbL <- verbPhrase.interpret(cont)
    } yield Abs(x, i, And(App(w, x), App(verbL, x)))

  override def gender: Gender = word match {
    case "Mary" => Female
    case "John" => Male
    case _      => Other
  }
}

final case class ReflexivePronounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word with NounPhraseWithVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              inspect(_.findAnaphoricReferent(Abs(x, i, App(nonHuman, x))).get)
            else if (word.toLowerCase == "he")
              inspect(_.findAnaphoricReferent(Abs(x, i, App(male, x))).get)
            else ???
      verbL <- verbPhrase.interpret(cont)
    } yield App(verbL, ref)

  override def gender: Gender = word match {
    case "he"  => Male
    case "she" => Female
    case "it"  => Other
    case _     => Other
  }
}
