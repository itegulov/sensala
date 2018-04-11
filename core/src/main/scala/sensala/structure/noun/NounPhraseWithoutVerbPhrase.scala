package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{False, True}
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.{Property, WordNetPropertyExtractor}

trait NounPhraseWithoutVerbPhrase extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      contL <- cont
    } yield named(x, w) /\: contL

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female), Property(person))
    case "John" => List(Property(male), Property(person))
    case _      => List(Property(animal))
  }
}

case class CommonNoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      contL <- cont
    } yield w(x) /\: contL

  override def properties: List[Property] = WordNetPropertyExtractor.extractProperties(word)
}

final case class ReflexivePronoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      ref <- PronounPropertyMap.map.get(word.toLowerCase) match {
              case Some(property) =>
                gets[NLFx, Context, E](_.findAnaphoricReferent(x, property(x)).get)
              case None => left[NLFx, NLError, E](NLUnexpectedWord(word))
            }
      _     <- putEntity(ref.asInstanceOf[Var]) // FIXME: remove type casting?
      contL <- cont
    } yield contL

  override def properties: List[Property] =
    PronounPropertyMap.map
      .get(word.toLowerCase)
      .map(sym => List(Property(sym)))
      .getOrElse(List(Property(animal)))
}

final case class DemonstrativePronoun(
  word: String
) extends Word
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      e     <- gets[NLFx, Context, E](_.findAnaphoricEvent(x, truth(x)).get)
      _     <- putEntity(e.asInstanceOf[Var])
      contL <- cont
    } yield contL

  override def properties: List[Property] = List.empty
}
