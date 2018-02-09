package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.Property
import sensala.structure.types._
import sensala.structure.verb.VerbPhrase

trait NounPhraseWithVerbPhrase extends NounPhrase {
  val verbPhrase: VerbPhrase
}

final case class ProperNounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word
    with NounPhraseWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      w = Sym(word)
      verbL <- verbPhrase.interpret(cont)
    } yield Abs(x, entity, w(x) /\: verbL(x))

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female))
    case "John" => List(Property(male))
    case _      => List(Property(nonHuman))
  }
}

final case class ReflexivePronounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word
    with NounPhraseWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, nonHuman(x)).get)
            else if (word.toLowerCase == "he")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, male(x) \/: human(x)).get)
            else
              left[NLFx, NLError, E](NLUnexpectedWord(word))
      verbL <- verbPhrase.interpret(cont)
    } yield verbL(ref)

  override def properties: List[Property] = word match {
    case "he"  => List(Property(male))
    case "she" => List(Property(female))
    case "it"  => List(Property(nonHuman))
    case _     => List(Property(nonHuman))
  }
}
