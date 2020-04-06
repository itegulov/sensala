package sensala.parser.english

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.instances.list._
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.process.Morphology
import edu.stanford.nlp.semgraph.SemanticGraph
import sensala.models.nl._
import sensala.parser.english.EnglishSensalaGrammaticalRelations._
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.ParserUtil._

import scala.collection.convert.ImplicitConversionsToScala._
import scala.util.{Failure, Success, Try}

class NounPhraseParser[F[_]: Monad: HandleParserError: PronounParser: VerbPhraseParser] {
  sealed trait CommonNounDeterminer
  case object Existential extends CommonNounDeterminer
  case object Forall      extends CommonNounDeterminer
  case object The         extends CommonNounDeterminer

  private def parseCommonNoun(
    nounPhrase: IndexedWord
  )(implicit graph: SemanticGraph): F[CommonNounDeterminer] = {
    require(nounPhrase.tag == "NN")
    val children = graph.childPairs(nounPhrase).toList.map(pairToTuple)
    val determiners = children.collect {
      case (rel, word) if rel == Det => word
    }
    determiners match {
      case Nil =>
        // FIXME: Currently treating all common nouns without determiners as indefinite
        Existential.pure[F].widen
      case x :: Nil
          if x.word.toLowerCase == "a" || x.word.toLowerCase == "an" || x.word.toLowerCase == "some" =>
        Existential.pure[F].widen
      case x :: Nil if Set("every", "each").contains(x.word.toLowerCase) =>
        Forall.pure[F].widen
      case x :: Nil if x.word.toLowerCase == "the" =>
        The.pure[F].widen
      case x :: Nil =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown determiner: ${x.word}"))
      case _ =>
        HandleParserError[F].raise(
          InvalidDiscourse(s"Multiple determiners: ${determiners.map(_.word).mkString(" ")}")
        )
    }
  }

  private def parsePluralCommonNoun(
    nounPhrase: IndexedWord
  )(implicit graph: SemanticGraph): F[CommonNounDeterminer] = {
    require(nounPhrase.tag == "NNS")
    val children = graph.childPairs(nounPhrase).toList.map(pairToTuple)
    val determiners = children.collect {
      case (rel, word) if rel == Det => word
    }
    determiners match {
      case Nil =>
        // FIXME: Currently treating all common nouns without determiners as indefinite
        Existential.pure[F].widen
      case x :: Nil if x.word.toLowerCase == "some" =>
        Existential.pure[F].widen
      case x :: Nil if x.word.toLowerCase == "all" =>
        Forall.pure[F].widen
      case x :: Nil if x.word.toLowerCase == "the" =>
        The.pure[F].widen
      case x :: Nil =>
        HandleParserError[F].raise(InvalidDiscourse(s"Unknown determiner: ${x.word}"))
      case _ =>
        HandleParserError[F].raise(
          InvalidDiscourse(s"Multiple determiners: ${determiners.map(_.word).mkString(" ")}")
        )
    }
  }

  private def parseNumeralModifiers(
    nounPhrase: IndexedWord
  )(implicit graph: SemanticGraph): F[NounPhrase] = {
    require(nounPhrase.tag == "NNS")
    val children = graph.childPairs(nounPhrase).toList.map(pairToTuple)
    val numModifiers = children.collect {
      case (rel, word) if rel == NumMod => word
    }
    val nonpluralWord = Morphology.stemStatic(nounPhrase.word, nounPhrase.tag)
    numModifiers match {
      case Nil =>
        PluralCommonNoun(nonpluralWord.word).pure[F].widen
      case numModifier :: Nil =>
        Try(numModifier.word.toInt) match {
          case Success(value) =>
            PluralNumericCommonNoun(nonpluralWord.word, value).pure[F].widen
          case Failure(exception) =>
            Try(NumberNormalizer.wordToNumber(numModifier.word)) match {
              case Success(value) =>
                PluralNumericCommonNoun(nonpluralWord.word, value.intValue).pure[F].widen
              case Failure(e) =>
                HandleParserError[F].raise(
                  InvalidDiscourse(s"${numModifier.word} is not a number: ${e.getMessage}")
                )
            }
        }
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Multiple numeric modifiers are unsupported"))
    }
  }

  private def parseAdjectiveNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): F[NounPhrase] = {
    val modifiers = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdjMod => word
    }
    for {
      mods <- modifiers.map {
               case x if x.tag == "JJ" => Adjective(x.word).pure[F]
               case x =>
                 HandleParserError[F].raise[Adjective](
                   InvalidDiscourse(s"Unknown modifier: (${x.tag}) ${x.word}")
                 )
             }.sequence[F, Adjective]
    } yield mods.foldRight(nounPhrase)((adj, np) => AdjectiveNounPhrase(adj, np))
  }

  private def parseWhNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): F[NounPhrase] = {
    val refs = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == Ref => word
    }
    val relativeClauses = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == RelClMod => word
    }
    // FIXME: References and relative clauses can be ordered differently
    refs.zip(relativeClauses).foldM(nounPhrase) {
      case (prevNp, (ref, relClause)) if ref.word.toLowerCase == "who" =>
        VerbPhraseParser[F].parseVerbPhrase(relClause).map(WhNounPhrase(_, prevNp))
      case (prevNp, (ref, relClause)) if ref.word.toLowerCase == "that" =>
        VerbPhraseParser[F].parseVerbPhrase(relClause).map(RelativeClausePhrase("that", _, prevNp))
    }
  }

  private def parsePrepositionalNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): F[NounPhrase] = {
    val prepositions = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if NomMod.isAncestor(rel) || rel == NomModPoss => (rel, word)
    }
    prepositions.foldM(nounPhrase) {
      case (prevNp, (rel, preposition)) if rel == NomModOn =>
        for {
          prepositionNounPhrase <- parseNounPhrase(preposition)
          prepositionGraphMap = graph
            .childPairs(preposition)
            .map(pairToTuple)
            .toMap
          caseWord <- prepositionGraphMap.get(Case) match {
                       case Some(word) => word.pure[F]
                       case None =>
                         HandleParserError[F].raise[IndexedWord](
                           InvalidDiscourse("Invalid preposition: no case word")
                         )
                     }
        } yield NounPhrasePreposition(InPhrase(caseWord.word, prepositionNounPhrase), prevNp)
      case (prevNp, (rel, preposition)) if rel == NomModPoss =>
        for {
          prepositionNounPhrase <- parseNounPhrase(preposition)
        } yield NounPhrasePreposition(PossessionPhrase(prepositionNounPhrase), prevNp)
      case _ =>
        HandleParserError[F].raise[NounPhrase](InvalidDiscourse("Illegal nominal modifier"))
    }
  }

  def parseNounPhrase(
    nounTree: IndexedWord
  )(implicit graph: SemanticGraph): F[NounPhrase] =
    nounTree.tag match {
      case "NN" =>
        HandleParserError[F].attempt(PronounParser[F].parseIndefinitePronoun(nounTree)).flatMap {
          case Right(pronoun) =>
            pronoun.pure[F].widen
          case Left(_) =>
            HandleParserError[F].attempt(parseCommonNoun(nounTree)).flatMap {
              case Right(Existential) =>
                for {
                  adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                          nounTree,
                                          CommonNoun(nounTree.word)
                                        )
                  whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
                  prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
                } yield ExistentialQuantifier(prepNounPhrase)
              case Right(Forall) =>
                for {
                  adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                          nounTree,
                                          CommonNoun(nounTree.word)
                                        )
                  whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
                  prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
                } yield ForallQuantifier(prepNounPhrase)
              case Right(The) =>
                for {
                  adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                          nounTree,
                                          CommonNoun(nounTree.word)
                                        )
                  whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
                  prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
                } yield DefiniteNounPhrase(prepNounPhrase)
              case Left(error) =>
                HandleParserError[F].raise(error)
            }
        }
      case "NNS" =>
        HandleParserError[F].attempt(parsePluralCommonNoun(nounTree)).flatMap {
          case Right(Existential) =>
            for {
              numericPlural <- parseNumeralModifiers(nounTree)
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      numericPlural
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield ExistentialQuantifier(prepNounPhrase)
          case Right(Forall) =>
            for {
              numericPlural <- parseNumeralModifiers(nounTree)
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      numericPlural
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield ForallQuantifier(prepNounPhrase)
          case Right(The) =>
            for {
              numericPlural <- parseNumeralModifiers(nounTree)
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      numericPlural
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield DefiniteNounPhrase(prepNounPhrase)
          case Left(error) =>
            HandleParserError[F].raise(error)
        }
      case "NNP" =>
        val ner = Option(nounTree.ner()).flatMap(parseNer)
        val gender =
          Option(nounTree.get(classOf[CoreAnnotations.GenderAnnotation])).flatMap(parseGender)
        val properNoun = ProperNoun(nounTree.word, ner, gender)
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, properNoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield ExistentialQuantifier(prepNounPhrase)
      case "PRP" =>
        for {
          pronoun             <- PronounParser[F].parsePersonalOrReflexivePronoun(nounTree)
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, pronoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "PRP$" =>
        for {
          possessivePronoun   <- PronounParser[F].parsePossessivePronoun(nounTree)
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, possessivePronoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "DT" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                  nounTree,
                                  DemonstrativePronoun(nounTree.word)
                                )
          whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "CD" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                  nounTree,
                                  NumericNoun(
                                    NumberNormalizer.wordToNumber(nounTree.word).intValue()
                                  )
                                )
          whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case _ =>
        HandleParserError[F].raise(
          InvalidDiscourse(s"Unknown noun phrase: (${nounTree.tag}) ${nounTree.word}")
        )
    }
}

object NounPhraseParser {
  def apply[F[_]](implicit ev: NounPhraseParser[F]): NounPhraseParser[F] = ev
}
