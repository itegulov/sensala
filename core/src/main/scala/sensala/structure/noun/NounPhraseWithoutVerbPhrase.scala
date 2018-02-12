package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.{Property, PropertyExtractor}

trait NounPhraseWithoutVerbPhrase extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- getEntity
      w = Sym(word)
      contL <- cont
    } yield named(x, w) /\: contL

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female))
    case "John" => List(Property(male))
    case _      => List(Property(nonHuman))
  }
}

case class CommonNoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- getEntity
      w = Sym(word)
      contL <- cont
    } yield w(x) /\: contL

  override def properties: List[Property] = PropertyExtractor.extractProperties(word)
}

final case class ReflexivePronoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, nonHuman(x)).get)
            else if (word.toLowerCase == "he")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, male(x)).get)
            else
              left[NLFx, NLError, E](NLUnexpectedWord(word))
      _ <- putEntity(ref.asInstanceOf[Var]) // FIXME: remove type casting?
      contL <- cont
    } yield contL

  override def properties: List[Property] = word match {
    case "he"  => List(Property(male))
    case "she" => List(Property(female))
    case "it"  => List(Property(nonHuman))
    case _     => List(Property(nonHuman))
  }
}
