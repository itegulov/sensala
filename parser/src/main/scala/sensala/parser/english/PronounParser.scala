package sensala.parser.english

import cats.Monad
import cats.implicits._
import edu.stanford.nlp.ling.IndexedWord
import sensala.models.nl._
import sensala.parser.english.ParserError.HandleParserError

class PronounParser[F[_]: Monad: HandleParserError] {
  def parseIndefinitePronoun(word: IndexedWord): F[IndefinitePronoun] =
    word.word.toLowerCase match {
      case "nobody" => NegativePersonSingularIndefinitePronoun(word.word).pure[F].widen
      case "everyone" | "everybody" =>
        UniversalPersonSingularIndefinitePronoun(word.word).pure[F].widen
      case "someone" | "somebody" =>
        ExistentialPersonSingularIndefinitePronoun(word.word).pure[F].widen
      case "nothing"    => NegativePersonSingularIndefinitePronoun(word.word).pure[F].widen
      case "everything" => UniversalPersonSingularIndefinitePronoun(word.word).pure[F].widen
      case "something"  => ExistentialPersonSingularIndefinitePronoun(word.word).pure[F].widen
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown indefinite pronoun: ${word.word}"))
    }

  def parsePersonalPronoun(word: IndexedWord): F[PersonalPronoun] =
    word.word.toLowerCase match {
      case "i" | "me"      => FirstPersonSingularPersonalPronoun(word.word).pure[F].widen
      case "you"           => SecondPersonSingularPersonalPronoun(word.word).pure[F].widen
      case "he" | "him"    => ThirdPersonSingularPersonalPronoun(word.word, Masculine).pure[F].widen
      case "she" | "her"   => ThirdPersonSingularPersonalPronoun(word.word, Feminine).pure[F].widen
      case "it"            => ThirdPersonSingularPersonalPronoun(word.word, Neuter).pure[F].widen
      case "we" | "us"     => FirstPersonPluralPersonalPronoun(word.word).pure[F].widen
      case "they" | "them" => ThirdPersonPluralPersonalPronoun(word.word).pure[F].widen
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown personal pronoun: ${word.word}"))
    }

  def parseReflexivePronoun(word: IndexedWord): F[ReflexivePronoun] =
    word.word.toLowerCase match {
      case "myself"     => FirstPersonSingularReflexivePronoun(word.word).pure[F].widen
      case "yourself"   => SecondPersonSingularReflexivePronoun(word.word).pure[F].widen
      case "himself"    => ThirdPersonSingularReflexivePronoun(word.word, Masculine).pure[F].widen
      case "herself"    => ThirdPersonSingularReflexivePronoun(word.word, Feminine).pure[F].widen
      case "itself"     => ThirdPersonSingularReflexivePronoun(word.word, Neuter).pure[F].widen
      case "ourselves"  => FirstPersonPluralReflexivePronoun(word.word).pure[F].widen
      case "themselves" => ThirdPersonPluralReflexivePronoun(word.word).pure[F].widen
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown reflexive pronoun: ${word.word}"))
    }

  def parsePersonalOrReflexivePronoun(word: IndexedWord): F[Pronoun] =
    HandleParserError[F].attempt(parsePersonalPronoun(word)).flatMap {
      case Right(pronoun) => pronoun.pure[F].widen
      case Left(_)        => parseReflexivePronoun(word).widen
    }

  def parsePossessivePronoun(word: IndexedWord): F[PossessivePronoun] =
    word.word.toLowerCase match {
      case "my" | "mine"      => FirstPersonSingularPossessivePronoun(word.word).pure[F].widen
      case "your" | "yours"   => SecondPersonSingularPossessivePronoun(word.word).pure[F].widen
      case "his"              => ThirdPersonSingularPossessivePronoun(word.word, Masculine).pure[F].widen
      case "her" | "hers"     => ThirdPersonSingularPossessivePronoun(word.word, Feminine).pure[F].widen
      case "its"              => ThirdPersonSingularPossessivePronoun(word.word, Neuter).pure[F].widen
      case "our" | "ours"     => FirstPersonPluralPossessivePronoun(word.word).pure[F].widen
      case "their" | "theirs" => ThirdPersonPluralPossessivePronoun(word.word).pure[F].widen
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown possessive pronoun: ${word.word}"))
    }
}

object PronounParser {
  def apply[F[_]](implicit ev: PronounParser[F]): PronounParser[F] = ev
}
