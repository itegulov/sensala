package sensala.structure

import org.aossie.scavenger.expression.Sym
import sensala.property.{Property, WordNetPropertyExtractor}

sealed trait NL

final case class Discourse(sentences: List[Sentence]) extends NL

final case class Sentence(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NL

/*
 * Wh Phrases
 */

sealed trait WhPhrase extends NL
final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhrase {
  override def properties                         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

/*
 * Verb Phrases
 */

sealed trait VerbPhrase                                                         extends NL
final case class IntransitiveVerb(word: String)                                 extends VerbPhrase
final case class TransitiveVerb(word: String, obj: NounPhrase)                  extends VerbPhrase
final case class VerbAdjectivePhrase(verb: String, adj: Adjective)              extends VerbPhrase
final case class VerbInPhrase(preposition: PrepositionalPhrase, vp: VerbPhrase) extends VerbPhrase
final case class VerbPhraseAnaphora(phrase: String, voice: VerbVoice)           extends VerbPhrase
final case class VerbSentencePhrase(word: String, sentence: Sentence)           extends VerbPhrase

/*
 * Prepositional Phrases
 */

sealed trait PrepositionalPhrase extends NL {
  val verbWord: String
  val nounPhrase: NounPhrase
}
final case class InPhrase(verbWord: String, nounPhrase: NounPhrase) extends PrepositionalPhrase
final case class PossessionPhrase(nounPhrase: NounPhrase) extends PrepositionalPhrase {
  val verbWord = "owns"
}

/*
 * Noun Phrases
 */

sealed trait NounPhrase extends NL {
  def properties: List[Property]
  def definiteProperties: List[Property]
}

// Basic Noun Phrases

sealed trait NounPhraseBasic extends NounPhrase

final case class ProperNoun(
  word: String,
  typ: Option[NamedEntityType],
  gender: Option[NamedEntityGender]
) extends NounPhraseBasic {
  private def typProperty: List[Property] = typ match {
    case Some(Location)     => List(Property(x => location(x)))
    case Some(Person)       => List(Property(x => person(x)))
    case Some(Organization) => List(Property(x => organization(x)))
    case Some(Money)        => List(Property(x => money(x)))
    case Some(Percent)      => List(Property(x => percent(x)))
    case Some(Date)         => List(Property(x => date(x)))
    case Some(Time)         => List(Property(x => time(x)))
    case None               => List()
  }

  private def genderProperty: List[Property] = gender match {
    case Some(Male)   => List(Property(x => male(x)))
    case Some(Female) => List(Property(x => female(x)))
    case None         => List()
  }

  override def properties: List[Property]         = typProperty ++ genderProperty
  override def definiteProperties: List[Property] = properties
}

final case class CommonNoun(
  word: String
) extends NounPhraseBasic {
  override def properties: List[Property]         = WordNetPropertyExtractor.extractProperties(word)
  override def definiteProperties: List[Property] = List(Property(x => Sym(word)(x)))
}

// Preposition Noun Phrases

final case class NounPhrasePreposition(
  prepositionalPhrase: PrepositionalPhrase,
  nounPhrase: NounPhrase
) extends NounPhrase {
  // TODO: Add preposition property
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

// Quantifier Noun Phrases

sealed trait NounPhraseQuantifier extends NounPhrase

final case class ForallQuantifier(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class ExistentialQuantifier(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

final case class DefiniteNounPhrase(nounPhrase: NounPhrase) extends NounPhraseQuantifier {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}

/*
 * Pronouns
 */

sealed trait Pronoun extends NounPhrase {
  val word: String
}

// Demonstrative Pronouns

final case class DemonstrativePronoun(word: String) extends Pronoun {
  override def properties: List[Property]         = List.empty
  override def definiteProperties: List[Property] = List.empty
}

// Indefinite Pronouns

sealed trait IndefinitePronoun extends Pronoun

sealed trait SingularIndefinitePronoun extends IndefinitePronoun

sealed trait PersonSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List(Property(x => person(x)))

  override def definiteProperties: List[Property] = properties
}
final case class NegativePersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class UniversalPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class ExistentialPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun

sealed trait ThingSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
final case class NegativeThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun
final case class UniversalThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun
final case class ExistentialThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun

// Personal Pronouns

sealed abstract class PersonalPronoun extends Pronoun

sealed trait SingularPersonalPronoun extends PersonalPronoun

final case class FirstPersonSingularPersonalPronoun(
  word: String
) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularPersonalPronoun(
  word: String
) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularPersonalPronoun(
  word: String,
  gender: PronounGender
) extends SingularPersonalPronoun {
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

final case class FirstPersonPluralPersonalPronoun(
  word: String
) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralPersonalPronoun(
  word: String
) extends SingularPersonalPronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralPersonalPronoun(
  word: String
) extends SingularPersonalPronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}

// Possessive Pronouns

sealed trait PossessivePronoun extends Pronoun

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

// Reflexive Pronouns

sealed abstract class ReflexivePronoun extends Pronoun

sealed trait SingularReflexivePronoun extends ReflexivePronoun

final case class FirstPersonSingularReflexivePronoun(
  word: String
) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonSingularReflexivePronoun(
  word: String
) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonSingularReflexivePronoun(
  word: String,
  gender: PronounGender
) extends SingularReflexivePronoun {
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

final case class FirstPersonPluralReflexivePronoun(
  word: String
) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => speaker(x)))

  override def definiteProperties: List[Property] = properties
}

final case class SecondPersonPluralReflexivePronoun(
  word: String
) extends SingularReflexivePronoun {
  override def properties: List[Property] = List(Property(x => interlocutor(x)))

  override def definiteProperties: List[Property] = properties
}

final case class ThirdPersonPluralReflexivePronoun(
  word: String
) extends SingularReflexivePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}

/*
 * Adverb Phrases
 */

sealed trait AdverbPhrase extends NL {
  val adverb: Adverb
}

final case class VerbAdverbPhrase(
  adverb: Adverb,
  verbPhrase: VerbPhrase
) extends AdverbPhrase
    with VerbPhrase

/*
 * Adjective Phrases
 */
sealed trait AdjectivePhrase extends NL {
  val adjective: Adjective
}

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectivePhrase
    with NounPhrase {
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
