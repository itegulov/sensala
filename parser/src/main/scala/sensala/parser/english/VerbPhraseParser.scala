package sensala.parser.english

import cats.Monad
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.instances.list._
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.semgraph.SemanticGraph
import sensala.models.nl._
import sensala.parser.english.SensalaGrammaticalRelations._
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.ParserUtil._

import scala.jdk.CollectionConverters._

class VerbPhraseParser[F[_]: Monad: HandleParserError: NounPhraseParser: DiscourseParser] {
  def parseAdverbialClauseVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): F[VerbPhrase] = {
    val adverbialClauses = graph.childPairs(verbTree).asScala.toList.map(pairToTuple).collect {
      case (rel, word) if AdvclMod.isAncestor(rel) => (rel.getSpecific, word)
    }
    val adverbs = graph.childPairs(verbTree).asScala.toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdvMod => word
    }
    adverbialClauses match {
      case Nil =>
        val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
        adverbMods.foldRight(verbPhrase)(VerbAdverbPhrase.apply).pure[F]
      case (mark, adverbialClause) :: Nil =>
        val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
        for {
          clause <- DiscourseParser[F].parseSentence(adverbialClause)
        } yield VerbAdverbialClausePhrase(mark, clause, adverbMods, verbPhrase)
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Multiple adverbial clauses are unsupported"))
    }
  }

  def parsePrepositionalVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): F[VerbPhrase] = {
    val prepositions = graph.childPairs(verbTree).asScala.toList.map(pairToTuple).collect {
      case (rel, word) if NomMod.isAncestor(rel) => word
    }
    for {
      prepositionModifiers <- prepositions.map { preposition =>
                               for {
                                 prepositionNounPhrase <- NounPhraseParser[F].parseNounPhrase(
                                                           preposition
                                                         )
                                 prepositionGraphMap = graph
                                   .childPairs(preposition)
                                   .asScala
                                   .map(pairToTuple)
                                   .toMap
                                 caseWord <- prepositionGraphMap.get(Case) match {
                                              case Some(word) => word.pure[F]
                                              case None =>
                                                HandleParserError[F].raise[IndexedWord](
                                                  InvalidDiscourse(
                                                    "Invalid preposition: no case word"
                                                  )
                                                )
                                            }
                               } yield InPhrase(caseWord.word, prepositionNounPhrase)
                             }.sequence[F, InPhrase]
    } yield prepositionModifiers.foldRight(verbPhrase)(VerbInPhrase.apply)
  }

  def parseVerbPhrase(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): F[VerbPhrase] =
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap         = graph.childPairs(verbTree).asScala.map(pairToTuple).toMap
        val objOpt              = childrenMap.get(DObj)
        val clausalComponentOpt = childrenMap.get(CComp)
        (objOpt, clausalComponentOpt) match {
          case (Some(_), Some(_)) =>
            HandleParserError[F].raise(
              InvalidDiscourse(
                "Illegal verb phrase: object and clausal component cannot be specified together"
              )
            )
          case (Some(obj), None) =>
            for {
              objPhrase <- NounPhraseParser[F].parseNounPhrase(obj)
              adverbVerbPhrase <- parseAdverbialClauseVerbPhrase(
                                   verbTree,
                                   TransitiveVerb(verbTree.word, objPhrase)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
          case (None, Some(clausalComponent)) =>
            for {
              clausalSentence <- DiscourseParser[F].parseSentence(clausalComponent)
              adverbVerbPhrase <- parseAdverbialClauseVerbPhrase(
                                   verbTree,
                                   VerbSentencePhrase(verbTree.word, clausalSentence)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
          case (None, None) =>
            for {
              adverbVerbPhrase <- parseAdverbialClauseVerbPhrase(
                                   verbTree,
                                   IntransitiveVerb(verbTree.word)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
        }
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Invalid verb phrase"))
    }

  def parseVerbPhrasePassive(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): F[VerbPhrase] =
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap = graph.childPairs(verbTree).asScala.map(pairToTuple).toMap
        val passSubjOpt = childrenMap.get(NSubjPass)
        passSubjOpt match {
          case Some(passSubj) =>
            for {
              passSubjPhrase <- NounPhraseParser[F].parseNounPhrase(passSubj)
            } yield TransitiveVerb(verbTree.word, passSubjPhrase)
          case None =>
            IntransitiveVerb(verbTree.word).pure[F].widen
        }
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Invalid passive verb phrase"))
    }
}

object VerbPhraseParser {
  def apply[F[_]](implicit ev: VerbPhraseParser[F]): VerbPhraseParser[F] = ev
}
