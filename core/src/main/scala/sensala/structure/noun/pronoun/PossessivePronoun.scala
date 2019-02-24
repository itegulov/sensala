package sensala.structure.noun.pronoun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}


sealed abstract class PossessivePronoun[F[_]: Monad: Context: LocalContext] extends Pronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      ref   <- Context[F].findAnaphoricEntityUnsafe(properties)
      _     <- LocalContext[F].putEntity(ref)
      contL <- cont
    } yield contL
}

sealed trait SingularPossessivePronoun[F[_]] extends PossessivePronoun[F]

final case class FirstPersonSingularPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String,
  gender: PronounGender
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List(
    gender match {
      // For now we are using generic masculine pronoun (e.g. gender-neutral semantics of the pronoun "he")
      case Masculine => Property(x => person(x))
      case Feminine  => Property(x => female(x))
      case Neuter    => Property(x => animal(x))
    }
  )

  override def definiteProperties: List[Property] = properties
}

sealed trait PluralPossessivePronoun[F[_]] extends PossessivePronoun[F]

final case class FirstPersonPluralPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralPossessivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularPossessivePronoun[F] {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
