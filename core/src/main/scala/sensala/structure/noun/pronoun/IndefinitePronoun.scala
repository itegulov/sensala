package sensala.structure.noun.pronoun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.E
import org.aossie.scavenger.expression.formula._
import sensala.property.Property
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types._

sealed trait IndefinitePronoun[F[_]] extends Pronoun[F]

sealed trait SingularIndefinitePronoun[F[_]] extends IndefinitePronoun[F]

sealed trait PersonSingularIndefinitePronoun[F[_]] extends SingularIndefinitePronoun[F] {
  override def properties: List[Property] = List(Property(x => person(x)))

  override def definiteProperties: List[Property] = properties
}
final case class NegativePersonSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends PersonSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield ~Ex(x, entity, contL)
}
final case class UniversalPersonSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends PersonSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield All(x, entity, contL)
}
final case class ExistentialPersonSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends PersonSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield Ex(x, entity, contL)
}

sealed trait ThingSingularIndefinitePronoun[F[_]] extends SingularIndefinitePronoun[F] {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
final case class NegativeThingSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends ThingSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield ~Ex(x, entity, contL)
}
final case class UniversalThingSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends ThingSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield All(x, entity, contL)
}
final case class ExistentialThingSingularIndefinitePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends ThingSingularIndefinitePronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- Context[F].bindFreeVar
      _     <- Context[F].addEntity(x, properties)
      _     <- LocalContext[F].putEntity(x)
      contL <- cont
    } yield Ex(x, entity, contL)
}
