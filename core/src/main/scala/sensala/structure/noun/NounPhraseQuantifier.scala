package sensala.structure.noun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import sensala.property.Property
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types._

sealed trait NounPhraseQuantifier[F[_]] extends NounPhrase[F]

final case class ForallQuantifier[F[_]: Monad: Context: LocalContext](
  nounPhrase: NounPhrase[F]
) extends NounPhraseQuantifier[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      nounL <- nounPhrase.interpret(cont.map(~_))
    } yield All(x, entity, ~nounL)

  override def properties = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class ExistentialQuantifier[F[_]: Monad: Context: LocalContext](
  nounPhrase: NounPhrase[F]
) extends NounPhraseQuantifier[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      nounL <- nounPhrase.interpret(cont)
    } yield Ex(x, entity, nounL)

  override def properties = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class DefiniteNounPhrase[F[_]: Monad: Context: LocalContext](
  nounPhrase: NounPhrase[F]
) extends NounPhraseQuantifier[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      refOpt <- Context[F].findAnaphoricEntity(nounPhrase.definiteProperties)
      result <- refOpt match {
                 case Some(ref) =>
                   for {
                     _     <- LocalContext[F].putEntity(ref)
                     contL <- cont
                   } yield contL
                 case None =>
                   for {
                     x     <- Context[F].bindFreeVar
                     _     <- Context[F].addEntity(x, nounPhrase.properties)
                     _     <- LocalContext[F].putEntity(x)
                     nounL <- nounPhrase.interpret(cont)
                   } yield Ex(x, entity, nounL)
               }
    } yield result

  override def properties: List[Property] = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
