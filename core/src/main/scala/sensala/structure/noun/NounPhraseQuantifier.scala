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
}

final case class DefiniteNounPhrase(
  nounPhrase: NounPhrase
) extends NounPhraseQuantifier {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x          <- bindFreeVar
      properties = nounPhrase.properties.map(p => p.propertyExp(x)).foldLeft(True: E)(_ /\ _)
      ref        <- gets[NLFx, Context, E](_.findAnaphoricEntity(x, properties).get)
      _          <- putEntity(ref.asInstanceOf[Var])
      contL      <- cont
    } yield contL

  override def properties: List[Property] = nounPhrase.properties
}
