package sensala.structure.noun.pronoun

import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure._

sealed trait ReflexivePronoun extends Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      ref   <- findAnaphoricEntity(properties)
      _     <- putEntity(ref)
      contL <- cont
    } yield contL
}

sealed trait SingularReflexivePronoun extends ReflexivePronoun

final case class FirstPersonSingularReflexivePronoun(word: String) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularReflexivePronoun(word: String) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularReflexivePronoun(word: String, gender: PronounGender)
  extends SingularReflexivePronoun {
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

sealed trait PluralReflexivePronoun extends ReflexivePronoun

final case class FirstPersonPluralReflexivePronoun(word: String) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralReflexivePronoun(word: String) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralReflexivePronoun(word: String) extends SingularReflexivePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
