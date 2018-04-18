package sensala.structure.noun

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import org.atnos.eff.all._
import sensala.structure._
import sensala.property.Property
import sensala.structure.types._

sealed trait NounPhraseQuantifier extends NounPhrase

final case class ForallQuantifier(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addEntity(x, properties))
      _     <- putEntity(x)
      nounL <- nounPhrase.interpret(cont.map(~_))
    } yield All(x, entity, ~nounL)

  override def properties = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class ExistentialQuantifier(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- bindFreeVar
      _     <- modify[NLFx, Context](_.addEntity(x, properties))
      _     <- putEntity(x)
      nounL <- nounPhrase.interpret(cont)
    } yield Ex(x, entity, nounL)

  override def properties = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class DefiniteNounPhrase(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      refOpt <- findAnaphoricEntityOpt(nounPhrase.definiteProperties)
      result <- refOpt match {
                 case Some(ref) =>
                   for {
                     _     <- putEntity(ref)
                     contL <- cont
                   } yield contL
                 case None =>
                   for {
                     x     <- bindFreeVar
                     _     <- modify[NLFx, Context](_.addEntity(x, nounPhrase.properties))
                     _     <- putEntity(x)
                     nounL <- nounPhrase.interpret(cont)
                   } yield Ex(x, entity, nounL)
               }
    } yield result

  override def properties: List[Property] = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
