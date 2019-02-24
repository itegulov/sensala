package sensala.structure.noun.pronoun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}

sealed abstract class ReflexivePronoun[F[_]: Monad: Context: LocalContext] extends Pronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      ref   <- Context[F].findAnaphoricEntityUnsafe(properties)
      _     <- LocalContext[F].putEntity(ref)
      contL <- cont
    } yield contL
}

sealed trait SingularReflexivePronoun[F[_]] extends ReflexivePronoun[F]

final case class FirstPersonSingularReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularReflexivePronoun[F] {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularReflexivePronoun[F] {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String,
  gender: PronounGender
) extends SingularReflexivePronoun[F] {
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

sealed trait PluralReflexivePronoun[F[_]] extends ReflexivePronoun[F]

final case class FirstPersonPluralReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularReflexivePronoun[F] {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularReflexivePronoun[F] {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralReflexivePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends SingularReflexivePronoun[F] {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
