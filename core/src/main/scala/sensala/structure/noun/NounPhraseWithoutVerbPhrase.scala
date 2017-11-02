package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import sensala.structure._
import contextMonad._
import sensala.property.{Property, PropertyExtractor}

trait NounPhraseWithoutVerbPhrase extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, And(App(w, x), App(cont, x)))

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female))
    case "John" => List(Property(male))
    case _      => List(Property(nonHuman))
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

  override def properties: List[Property] = PropertyExtractor.extractProperties(word)
}

final case class ReflexivePronoun(
  word: String
) extends Word with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              inspect(_.findAnaphoricReferent(x, App(nonHuman, x)).get)
            else if (word.toLowerCase == "he")
              inspect(_.findAnaphoricReferent(x, App(male, x)).get)
            else ???
    } yield App(cont, ref)

  override def properties: List[Property] = word match {
    case "he" => List(Property(male))
    case "she" => List(Property(female))
    case "it" => List(Property(nonHuman))
    case _      => List(Property(nonHuman))
  }
}
