package sensala.models.nl

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
    with NounPhrase

/*
 * Relative Clauses
 */
final case class RelativeClausePhrase(
  word: String,
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhrase

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
final case class VerbComparativePhrase(comparative: String, obj: NounPhrase)    extends VerbPhrase
final case class VerbAdverbialClausePhrase(
  mark: String,
  adverbialClause: Sentence,
  adverbialModifiers: List[Adverb],
  verbPhrase: VerbPhrase
) extends VerbPhrase

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

sealed trait NounPhrase extends NL

// Basic Noun Phrases

sealed trait NounPhraseBasic extends NounPhrase

final case class ProperNoun(
  word: String,
  typ: Option[NamedEntityType],
  gender: Option[NamedEntityGender]
) extends NounPhraseBasic

final case class CommonNoun(
  word: String
) extends NounPhraseBasic

final case class PluralCommonNoun(
  word: String
) extends NounPhraseBasic

final case class PluralNumericCommonNoun(
  word: String,
  number: Int
) extends NounPhraseBasic

final case class NumericNoun(
  number: Int
) extends NounPhraseBasic

// Preposition Noun Phrases

final case class NounPhrasePreposition(
  prepositionalPhrase: PrepositionalPhrase,
  nounPhrase: NounPhrase
) extends NounPhrase

// Quantifier Noun Phrases

sealed trait NounPhraseQuantifier                              extends NounPhrase
final case class ForallQuantifier(nounPhrase: NounPhrase)      extends NounPhraseQuantifier
final case class ExistentialQuantifier(nounPhrase: NounPhrase) extends NounPhraseQuantifier
final case class DefiniteNounPhrase(nounPhrase: NounPhrase)    extends NounPhraseQuantifier

/*
 * Pronouns
 */

sealed trait Pronoun extends NounPhrase {
  val word: String
}

// Demonstrative Pronouns

final case class DemonstrativePronoun(word: String) extends Pronoun

// Indefinite Pronouns

sealed trait IndefinitePronoun extends Pronoun

sealed trait SingularIndefinitePronoun extends IndefinitePronoun

sealed trait PersonSingularIndefinitePronoun extends SingularIndefinitePronoun
final case class NegativePersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class UniversalPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class ExistentialPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun

sealed trait ThingSingularIndefinitePronoun extends SingularIndefinitePronoun
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
) extends SingularPersonalPronoun

final case class SecondPersonSingularPersonalPronoun(
  word: String
) extends SingularPersonalPronoun

final case class ThirdPersonSingularPersonalPronoun(
  word: String,
  gender: PronounGender
) extends SingularPersonalPronoun

sealed trait PluralPersonalPronoun extends PersonalPronoun

final case class FirstPersonPluralPersonalPronoun(
  word: String
) extends PluralPersonalPronoun

final case class SecondPersonPluralPersonalPronoun(
  word: String
) extends PluralPersonalPronoun

final case class ThirdPersonPluralPersonalPronoun(
  word: String
) extends PluralPersonalPronoun

// Possessive Pronouns

sealed trait PossessivePronoun extends Pronoun

sealed trait SingularPossessivePronoun extends PossessivePronoun

final case class FirstPersonSingularPossessivePronoun(
  word: String
) extends SingularPossessivePronoun

final case class SecondPersonSingularPossessivePronoun(
  word: String
) extends SingularPossessivePronoun

final case class ThirdPersonSingularPossessivePronoun(
  word: String,
  gender: PronounGender
) extends SingularPossessivePronoun

sealed trait PluralPossessivePronoun extends PossessivePronoun

final case class FirstPersonPluralPossessivePronoun(
  word: String
) extends PluralPossessivePronoun

final case class SecondPersonPluralPossessivePronoun(
  word: String
) extends PluralPossessivePronoun

final case class ThirdPersonPluralPossessivePronoun(
  word: String
) extends PluralPossessivePronoun

// Reflexive Pronouns

sealed abstract class ReflexivePronoun extends Pronoun

sealed trait SingularReflexivePronoun extends ReflexivePronoun

final case class FirstPersonSingularReflexivePronoun(
  word: String
) extends SingularReflexivePronoun

final case class SecondPersonSingularReflexivePronoun(
  word: String
) extends SingularReflexivePronoun

final case class ThirdPersonSingularReflexivePronoun(
  word: String,
  gender: PronounGender
) extends SingularReflexivePronoun

sealed trait PluralReflexivePronoun extends ReflexivePronoun

final case class FirstPersonPluralReflexivePronoun(
  word: String
) extends PluralReflexivePronoun

final case class SecondPersonPluralReflexivePronoun(
  word: String
) extends PluralReflexivePronoun

final case class ThirdPersonPluralReflexivePronoun(
  word: String
) extends PluralReflexivePronoun

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
    with NounPhrase
