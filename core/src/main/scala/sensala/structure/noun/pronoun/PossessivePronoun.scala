package sensala.structure.noun.pronoun

import sensala.property.Property
import sensala.structure._

sealed abstract class PossessivePronoun extends Pronoun

sealed trait SingularPossessivePronoun extends PossessivePronoun

final case class FirstPersonSingularPossessivePronoun(
  word: String
) extends SingularPossessivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularPossessivePronoun(
  word: String
) extends SingularPossessivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularPossessivePronoun(
  word: String,
  gender: PronounGender
) extends SingularPossessivePronoun {
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

sealed trait PluralPossessivePronoun extends PossessivePronoun

final case class FirstPersonPluralPossessivePronoun(
  word: String
) extends SingularPossessivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralPossessivePronoun(
  word: String
) extends SingularPossessivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralPossessivePronoun(
  word: String
) extends SingularPossessivePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
