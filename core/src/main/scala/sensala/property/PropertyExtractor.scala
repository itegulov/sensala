package sensala.property

import cats.effect.Sync
import cats.implicits._
import org.aossie.scavenger.expression.Sym
import sensala.structure._

final case class PropertyExtractor[F[_]: Sync]() {
  private def typProperty(typ: Option[NamedEntityType]): List[Property] =
    typ match {
      case Some(Location)     => List(Property(x => location(x)))
      case Some(Person)       => List(Property(x => person(x)))
      case Some(Organization) => List(Property(x => organization(x)))
      case Some(Money)        => List(Property(x => money(x)))
      case Some(Percent)      => List(Property(x => percent(x)))
      case Some(Date)         => List(Property(x => date(x)))
      case Some(Time)         => List(Property(x => time(x)))
      case None               => List()
    }

  private def genderProperty(gender: Option[NamedEntityGender]): List[Property] =
    gender match {
      case Some(Male)   => List(Property(x => male(x)))
      case Some(Female) => List(Property(x => female(x)))
      case None         => List()
    }

  def properties(np: NounPhrase): F[List[Property]] =
    np match {
      case WhNounPhrase(_, nounPhrase) =>
        properties(nounPhrase)
      case ProperNoun(_, typ, gender) =>
        (typProperty(typ) ++ genderProperty(gender)).pure[F]
      case CommonNoun(word) =>
        Sync[F].delay {
          WordNetPropertyExtractor.extractProperties(word)
        }
      case NounPhrasePreposition(_, nounPhrase) =>
        properties(nounPhrase)
      case ForallQuantifier(nounPhrase) =>
        properties(nounPhrase)
      case ExistentialQuantifier(nounPhrase) =>
        properties(nounPhrase)
      case DefiniteNounPhrase(nounPhrase) =>
        properties(nounPhrase)
      case DemonstrativePronoun(_) =>
        List.empty[Property].pure[F]
      case _: PersonSingularIndefinitePronoun =>
        List(Property(x => person(x))).pure[F]
      case _: ThingSingularIndefinitePronoun =>
        List.empty[Property].pure[F]
      case FirstPersonSingularPersonalPronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonSingularPersonalPronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonSingularPersonalPronoun(_, gender) =>
        List(
          gender match {
            // For now we are using generic masculine pronoun (e.g. gender-neutral semantics of the pronoun "he")
            case Masculine => Property(x => person(x))
            case Feminine  => Property(x => female(x))
            case Neuter    => Property(x => animal(x))
          }
        ).pure[F]
      case FirstPersonPluralPersonalPronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonPluralPersonalPronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonPluralPersonalPronoun(_) =>
        List.empty[Property].pure[F]
      case _: PluralPersonalPronoun =>
        List.empty[Property].pure[F]
      case FirstPersonSingularPossessivePronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonSingularPossessivePronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonSingularPossessivePronoun(_, gender) =>
        List(
          gender match {
            // For now we are using generic masculine pronoun (e.g. gender-neutral semantics of the pronoun "he")
            case Masculine => Property(x => person(x))
            case Feminine  => Property(x => female(x))
            case Neuter    => Property(x => animal(x))
          }
        ).pure[F]
      case FirstPersonPluralPossessivePronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonPluralPossessivePronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonPluralPossessivePronoun(_) =>
        List.empty[Property].pure[F]
      case FirstPersonSingularReflexivePronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonSingularReflexivePronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonSingularReflexivePronoun(_, gender) =>
        List(
          gender match {
            // For now we are using generic masculine pronoun (e.g. gender-neutral semantics of the pronoun "he")
            case Masculine => Property(x => person(x))
            case Feminine  => Property(x => female(x))
            case Neuter    => Property(x => animal(x))
          }
        ).pure[F]
      case FirstPersonPluralReflexivePronoun(_) =>
        List(Property(x => speaker(x))).pure[F]
      case SecondPersonPluralReflexivePronoun(_) =>
        List(Property(x => interlocutor(x))).pure[F]
      case ThirdPersonPluralReflexivePronoun(_) =>
        List.empty[Property].pure[F]
      case AdjectiveNounPhrase(_, nounPhrase) =>
        properties(nounPhrase)
    }

  def definiteProperties(np: NounPhrase): F[List[Property]] =
    np match {
      case WhNounPhrase(_, nounPhrase) =>
        definiteProperties(nounPhrase)
      case ProperNoun(_, typ, gender) =>
        properties(np)
      case CommonNoun(word) =>
        List(Property(x => Sym(word)(x))).pure[F]
      case NounPhrasePreposition(_, nounPhrase) =>
        definiteProperties(nounPhrase)
      case ForallQuantifier(nounPhrase) =>
        definiteProperties(nounPhrase)
      case ExistentialQuantifier(nounPhrase) =>
        definiteProperties(nounPhrase)
      case DefiniteNounPhrase(nounPhrase) =>
        definiteProperties(nounPhrase)
      case DemonstrativePronoun(_) =>
        List.empty[Property].pure[F]
      case _: Pronoun =>
        properties(np)
      case AdjectiveNounPhrase(_, nounPhrase) =>
        definiteProperties(nounPhrase)
    }
}

object PropertyExtractor {
  def apply[F[_]](implicit ev: PropertyExtractor[F]): PropertyExtractor[F] = ev
}
