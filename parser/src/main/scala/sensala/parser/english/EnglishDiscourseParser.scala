package sensala.parser.english

import cats.Monad
import cats.implicits._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.process.Morphology
import edu.stanford.nlp.semgraph.{SemanticGraph, SemanticGraphCoreAnnotations}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap
import sensala.parser.english.EnglishSensalaGrammaticalRelations._
import sensala.models.nl._
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.ParserUtil._

import scala.collection.convert.ImplicitConversionsToScala._

class EnglishDiscourseParser[F[_]: Monad: HandleParserError: NounPhraseParser: VerbPhraseParser] {
  private val logger = Logger[this.type]

  sealed trait CommonNounDeterminer
  case object Existential extends CommonNounDeterminer
  case object Forall      extends CommonNounDeterminer
  case object The         extends CommonNounDeterminer

  def parseActiveVoiceSentence(subj: IndexedWord, verbPhrase: VerbPhrase)(
    implicit graph: SemanticGraph
  ): F[Sentence] =
    for {
      subjPhrase <- NounPhraseParser[F].parseNounPhrase(subj)
    } yield Sentence(subjPhrase, verbPhrase)

  def parsePassiveVoiceSentence(
    verbRoot: IndexedWord
  )(implicit graph: SemanticGraph): F[Sentence] = {
    val children    = graph.childPairs(verbRoot).map(pairToTuple).toList
    val childrenMap = children.toMap
    val agents = children.collect {
      case (rel, word) if NomMod.isAncestor(rel) && rel.getSpecific == "agent" =>
        word
    }
    val auxPassOpt = childrenMap.get(AuxPass)
    (agents, auxPassOpt) match {
      case (Nil, _) =>
        HandleParserError[F].raise[Sentence](
          InvalidDiscourse("Illegal sentence: no subject and no agent")
        )
      case (agentWord :: Nil, Some(verbWord))
          if verbWord.word.toLowerCase == "am" || verbWord.word.toLowerCase == "is" || verbWord.word.toLowerCase == "are" =>
        for {
          agentPhrase <- NounPhraseParser[F].parseNounPhrase(agentWord)
          verbPhrase  <- VerbPhraseParser[F].parseVerbPhrasePassive(verbRoot)
        } yield Sentence(agentPhrase, verbPhrase)
      case (_, None) =>
        HandleParserError[F].raise[Sentence](
          InvalidDiscourse("Illegal passive voice sentence: no auxiliary verb")
        )
      case (_, _) =>
        HandleParserError[F].raise[Sentence](
          InvalidDiscourse("Illegal sentence: multiple agents")
        )
    }
  }

  def parseSentence(
    root: IndexedWord
  )(implicit graph: SemanticGraph): F[Sentence] =
    root.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val children    = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap = children.toMap
        val subjOpt     = childrenMap.get(NSubj)
        for {
          verbPhraseEither <- HandleParserError[F].attempt(
                               VerbPhraseParser[F].parseVerbPhrase(root)
                             )
          result <- (subjOpt, verbPhraseEither) match {
                     case (Some(subj), Right(verbPhrase)) =>
                       parseActiveVoiceSentence(subj, verbPhrase)
                     case (None, _) =>
                       parsePassiveVoiceSentence(root)
                     case (_, Left(error)) =>
                       HandleParserError[F].raise[Sentence](error)
                   }
        } yield result
      case "JJ" | "RB" =>
        val childrenMap = graph.childPairs(root).map(pairToTuple).toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt      = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            val stemCop = Morphology.stemStatic(cop.word, cop.tag)
            if (stemCop.word.toLowerCase == "be") {
              for {
                subjPhrase <- NounPhraseParser[F].parseNounPhrase(subj)
                verbPhrase = VerbAdjectivePhrase(cop.word, Adjective(root.word))
                adverbVerbPhrase <- VerbPhraseParser[F]
                                     .parseAdverbialClauseVerbPhrase(root, verbPhrase)
                prepositionalVerbPhrase <- VerbPhraseParser[F]
                                            .parsePrepositionalVerbPhrase(root, adverbVerbPhrase)
              } yield Sentence(subjPhrase, prepositionalVerbPhrase)
            } else {
              HandleParserError[F].raise(InvalidDiscourse(s"Invalid copular verb: ${cop.word}"))
            }
          case (None, _) =>
            HandleParserError[F].raise(InvalidDiscourse("Illegal sentence: no copular verb"))
          case (_, None) =>
            HandleParserError[F].raise(InvalidDiscourse("Illegal sentence: no subject"))
        }
      case "NN" | "NNP" | "PRP" =>
        val children    = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap = children.toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt      = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            val stemCop = Morphology.stemStatic(cop.word, cop.tag)
            if (stemCop.word.toLowerCase == "be") {
              for {
                subjPhrase <- NounPhraseParser[F].parseNounPhrase(subj)
                objPhrase  <- NounPhraseParser[F].parseNounPhrase(root)
              } yield Sentence(subjPhrase, TransitiveVerb(cop.word, objPhrase))
            } else {
              HandleParserError[F].raise(InvalidDiscourse(s"Invalid copular verb: ${cop.word}"))
            }
          case (Some(cop), None) =>
            val stemCop = Morphology.stemStatic(cop.word, cop.tag)
            if (stemCop.word.toLowerCase == "be") {
              children.collect {
                case (rel, word) if rel == AdvMod => word
              }.find(_.word.toLowerCase == "so") match {
                case Some(soWord) =>
                  for {
                    objPhrase <- NounPhraseParser[F].parseNounPhrase(root)
                  } yield Sentence(
                    objPhrase,
                    VerbPhraseAnaphora(soWord.word + " " + cop.word, Passive)
                  )
                case None =>
                  HandleParserError[F].raise(InvalidDiscourse("Invalid verb phrase anaphora"))
              }
            } else {
              HandleParserError[F].raise(InvalidDiscourse(s"Invalid copular verb: ${cop.word}"))
            }
          case (None, _) =>
            HandleParserError[F].raise(InvalidDiscourse("Illegal sentence: no copular verb"))
          case (_, None) =>
            HandleParserError[F].raise(InvalidDiscourse("Illegal sentence: no subject"))
        }
      case "RBR" =>
        val children    = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap = children.toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt      = childrenMap.get(Cop)
        val thanClauseOpt = children.find {
          case (rel, _) if NomMod.isAncestor(rel) && rel.getSpecific == "than" =>
            true
          case _ =>
            false
        }.map(_._2)
        (subjOpt, copOpt, thanClauseOpt) match {
          case (Some(subj), Some(cop), Some(thanClause)) =>
            val stemCop = Morphology.stemStatic(cop.word, cop.tag)
            if (stemCop.word.toLowerCase == "be") {
              for {
                subjPhrase <- NounPhraseParser[F].parseNounPhrase(subj)
                thanPhrase <- NounPhraseParser[F].parseNounPhrase(thanClause)
              } yield Sentence(subjPhrase, VerbComparativePhrase(root.word, thanPhrase))
            } else {
              HandleParserError[F].raise(InvalidDiscourse(s"Invalid copular verb: ${cop.word}"))
            }
          case _ =>
            HandleParserError[F].raise(InvalidDiscourse("Illegal comparative sentence"))
        }
    }

  private def transformVerbPhraseAnaphora(sentence: Sentence): Sentence =
    sentence.verbPhrase match {
      case VerbAdverbPhrase(Adverb("too"), IntransitiveVerb(word))
          if word.toLowerCase == "did" || word.toLowerCase == "does" =>
        Sentence(sentence.nounPhrase, VerbPhraseAnaphora(word + " too", Active))
      case VerbAdverbPhrase(Adverb(adWord), IntransitiveVerb(word))
          if adWord.toLowerCase == "so" || (word.toLowerCase == "did" || word.toLowerCase == "is") =>
        Sentence(sentence.nounPhrase, VerbPhraseAnaphora(adWord + " " + word, Passive))
      case _ =>
        sentence
    }

  private def parseSentence(sentence: CoreMap): F[Sentence] = {
    implicit val graph =
      sentence.get(classOf[SemanticGraphCoreAnnotations.EnhancedPlusPlusDependenciesAnnotation])
    logger.info("\n" + graph.toString)
    val root = graph.getFirstRoot
    parseSentence(root).map(transformVerbPhraseAnaphora)
  }

  private def annotateSentences(discourse: String): List[CoreMap] = {
    val document = new Annotation(discourse)
    EnglishSensalaStanfordParser.annotate(document)
    document.get(classOf[SentencesAnnotation]).toList
  }

  def buildPennTaggedTree(discourse: String): F[List[Tree]] = {
    val sentences = annotateSentences(discourse)
    sentences.map(_.get(classOf[TreeAnnotation])).pure[F]
  }

  def parse(discourse: String): F[Discourse] = {
    val sentences = annotateSentences(discourse)
    for {
      result <- sentences.map(parseSentence).sequence
    } yield Discourse(result)
  }
}

object EnglishDiscourseParser {
  def apply[F[_]](implicit ev: EnglishDiscourseParser[F]): EnglishDiscourseParser[F] = ev
}
