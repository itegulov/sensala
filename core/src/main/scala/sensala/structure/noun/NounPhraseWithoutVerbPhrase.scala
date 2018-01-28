package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import org.atnos.eff.all._
import sensala.property.{Property, PropertyExtractor}

trait NounPhraseWithoutVerbPhrase extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      w = Sym(word)
      contL <- cont
    } yield Abs(x, i, w(x) /\ contL(x))

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
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      w = Sym(word)
      contL <- cont
    } yield Abs(x, i, w(x) /\ contL(x))

  override def properties: List[Property] = PropertyExtractor.extractProperties(word)
}

final case class ReflexivePronoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      contL <- cont
      x     <- bindFreeVar
      ref <- if (word.toLowerCase == "it")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, nonHuman(x)).get)
            else if (word.toLowerCase == "he")
              gets[NLFx, Context, E](_.findAnaphoricReferent(x, male(x)).get)
            else
              left[NLFx, String, E]("Unknown anaphoric referent")
    } yield contL(ref)

  override def properties: List[Property] = word match {
    case "he"  => List(Property(male))
    case "she" => List(Property(female))
    case "it"  => List(Property(nonHuman))
    case _     => List(Property(nonHuman))
  }
}
