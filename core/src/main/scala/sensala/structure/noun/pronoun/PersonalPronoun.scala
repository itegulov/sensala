package sensala.structure.noun.pronoun

import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure._

sealed trait PersonalPronoun extends Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      ref   <- findAnaphoricEntity(properties)
      _     <- putEntity(ref)
      contL <- cont
    } yield contL
}

sealed trait SingularPersonalPronoun extends PersonalPronoun

final case class FirstPersonSingularPersonalPronoun(word: String) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularPersonalPronoun(word: String) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularPersonalPronoun(word: String, gender: PronounGender)
    extends SingularPersonalPronoun {
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

sealed trait PluralPersonalPronoun extends PersonalPronoun

final case class FirstPersonPluralPersonalPronoun(word: String) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralPersonalPronoun(word: String) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralPersonalPronoun(word: String) extends SingularPersonalPronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
