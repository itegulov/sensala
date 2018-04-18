package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.Property
import sensala.structure._

sealed trait Pronoun extends NounPhrase

final case class PossessivePronoun(
  word: String
) extends Word
  with Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      ref <- PronounPropertyMap.possessivePronouns.get(word.toLowerCase) match {
        case Some(property) =>
          findAnaphoricEntity(List(property))
        case None =>
          left[NLFx, NLError, Var](NLUnexpectedWord(word))
      }
      _     <- putEntity(ref)
      contL <- cont
    } yield contL

  override def properties: List[Property] =
    PronounPropertyMap.possessivePronouns
      .get(word.toLowerCase)
      .map(p => List(p))
      .getOrElse(List(Property(x => animal(x))))
  override def definiteProperties: List[Property] = properties
}

final case class ReflexivePronoun(
  word: String
) extends Word
  with Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      ref <- PronounPropertyMap.reflexivePronouns.get(word.toLowerCase) match {
        case Some(property) =>
          findAnaphoricEntity(List(property))
        case None =>
          left[NLFx, NLError, Var](NLUnexpectedWord(word))
      }
      _     <- putEntity(ref)
      contL <- cont
    } yield contL

  override def properties: List[Property] =
    PronounPropertyMap.reflexivePronouns
      .get(word.toLowerCase)
      .map(p => List(p))
      .getOrElse(List(Property(x => animal(x))))
  override def definiteProperties: List[Property] = properties
}

final case class DemonstrativePronoun(
  word: String
) extends Word
  with Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      e     <- findAnaphoricEvent(List.empty)
      _     <- putEntity(e)
      contL <- cont
    } yield contL

  override def properties: List[Property] = List.empty
  override def definiteProperties: List[Property] = List.empty
}
