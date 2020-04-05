package sensala.parser.english

import edu.stanford.nlp.ling.IndexedWord
import cats.syntax.either._
import sensala.models.nl._

object PronounParser {
  def parseIndefinitePronoun(word: IndexedWord): Either[String, IndefinitePronoun] =
    word.word.toLowerCase match {
      case "nobody"                 => Right(NegativePersonSingularIndefinitePronoun(word.word))
      case "everyone" | "everybody" => Right(UniversalPersonSingularIndefinitePronoun(word.word))
      case "someone" | "somebody"   => Right(ExistentialPersonSingularIndefinitePronoun(word.word))
      case "nothing"                => Right(NegativePersonSingularIndefinitePronoun(word.word))
      case "everything"             => Right(UniversalPersonSingularIndefinitePronoun(word.word))
      case "something"              => Right(ExistentialPersonSingularIndefinitePronoun(word.word))
      case _                        => Left(s"Unknown indefinite pronoun: ${word.word}")
    }

  def parsePersonalPronoun(word: IndexedWord): Either[String, PersonalPronoun] =
    word.word.toLowerCase match {
      case "i" | "me"      => Right(FirstPersonSingularPersonalPronoun(word.word))
      case "you"           => Right(SecondPersonSingularPersonalPronoun(word.word))
      case "he" | "him"    => Right(ThirdPersonSingularPersonalPronoun(word.word, Masculine))
      case "she" | "her"   => Right(ThirdPersonSingularPersonalPronoun(word.word, Feminine))
      case "it"            => Right(ThirdPersonSingularPersonalPronoun(word.word, Neuter))
      case "we" | "us"     => Right(FirstPersonPluralPersonalPronoun(word.word))
      case "they" | "them" => Right(ThirdPersonPluralPersonalPronoun(word.word))
      case _               => Left(s"Unknown personal pronoun: ${word.word}")
    }

  def parseReflexivePronoun(word: IndexedWord): Either[String, ReflexivePronoun] =
    word.word.toLowerCase match {
      case "myself"     => Right(FirstPersonSingularReflexivePronoun(word.word))
      case "yourself"   => Right(SecondPersonSingularReflexivePronoun(word.word))
      case "himself"    => Right(ThirdPersonSingularReflexivePronoun(word.word, Masculine))
      case "herself"    => Right(ThirdPersonSingularReflexivePronoun(word.word, Feminine))
      case "itself"     => Right(ThirdPersonSingularReflexivePronoun(word.word, Neuter))
      case "ourselves"  => Right(FirstPersonPluralReflexivePronoun(word.word))
      case "themselves" => Right(ThirdPersonPluralReflexivePronoun(word.word))
      case _            => Left(s"Unknown reflexive pronoun: ${word.word}")
    }

  def parsePersonalOrReflexivePronoun(word: IndexedWord): Either[String, Pronoun] =
    parsePersonalPronoun(word).orElse(parseReflexivePronoun(word))

  def parsePossessivePronoun(word: IndexedWord): Either[String, PossessivePronoun] =
    word.word.toLowerCase match {
      case "my" | "mine"      => Right(FirstPersonSingularPossessivePronoun(word.word))
      case "your" | "yours"   => Right(SecondPersonSingularPossessivePronoun(word.word))
      case "his"              => Right(ThirdPersonSingularPossessivePronoun(word.word, Masculine))
      case "her" | "hers"     => Right(ThirdPersonSingularPossessivePronoun(word.word, Feminine))
      case "its"              => Right(ThirdPersonSingularPossessivePronoun(word.word, Neuter))
      case "our" | "ours"     => Right(FirstPersonPluralPossessivePronoun(word.word))
      case "their" | "theirs" => Right(ThirdPersonPluralPossessivePronoun(word.word))
      case _                  => Left(s"Unknown possessive pronoun: ${word.word}")
    }
}
