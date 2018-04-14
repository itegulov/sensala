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
      x <- bindFreeVar
      ref <- PronounPropertyMap.possessivePronouns.get(word.toLowerCase) match {
        case Some(property) =>
          gets[NLFx, Context, Var](_.findAnaphoricEntity(x, property(x)).get)
        case None =>
          left[NLFx, NLError, Var](NLUnexpectedWord(word))
      }
      _     <- putEntity(ref)
      contL <- cont
    } yield contL

  override def properties: List[Property] =
    PronounPropertyMap.possessivePronouns
      .get(word.toLowerCase)
      .map(sym => List(Property(sym)))
      .getOrElse(List(Property(animal)))
}

final case class ReflexivePronoun(
  word: String
) extends Word
  with Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      ref <- PronounPropertyMap.reflexivePronouns.get(word.toLowerCase) match {
        case Some(property) =>
          gets[NLFx, Context, Var](_.findAnaphoricEntity(x, property(x)).get)
        case None =>
          left[NLFx, NLError, Var](NLUnexpectedWord(word))
      }
      _     <- putEntity(ref)
      contL <- cont
    } yield contL

  override def properties: List[Property] =
    PronounPropertyMap.reflexivePronouns
      .get(word.toLowerCase)
      .map(sym => List(Property(sym)))
      .getOrElse(List(Property(animal)))
}

final case class DemonstrativePronoun(
  word: String
) extends Word
  with Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      e     <- gets[NLFx, Context, Var](_.findAnaphoricEvent(x, truth(x)).get)
      _     <- putEntity(e)
      contL <- cont
    } yield contL

  override def properties: List[Property] = List.empty
}
