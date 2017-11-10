package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{And, Or}
import sensala.structure._
import contextMonad._
import sensala.property.Property
import sensala.structure.verb.VerbPhrase

trait NounPhraseWithVerbPhrase extends NounPhrase {
  val verbPhrase: VerbPhrase
}

final case class ProperNounVP(
  word: String, verbPhrase: VerbPhrase
) extends Word with NounPhraseWithVerbPhrase {
  override def interpret(cont: CState): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
      verbL <- verbPhrase.interpret(cont)
    } yield Abs(x, i, And(App(w, x), App(verbL, x)))

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female))
    case "John" => List(Property(male))
    case _      => List(Property(nonHuman))
  }
}

final case class ReflexivePronounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word with NounPhraseWithVerbPhrase {
  override def interpret(cont: CState): CState =
    for {
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              inspect(_.findAnaphoricReferent(x, App(nonHuman, x)).get)
            else if (word.toLowerCase == "he")
              inspect(_.findAnaphoricReferent(x, Or(App(male, x), App(human, x))).get)
            else ???
      verbL <- verbPhrase.interpret(cont)
    } yield App(verbL, ref)

  override def properties: List[Property] = word match {
    case "he" => List(Property(male))
    case "she" => List(Property(female))
    case "it" => List(Property(nonHuman))
    case _      => List(Property(nonHuman))
  }
}
