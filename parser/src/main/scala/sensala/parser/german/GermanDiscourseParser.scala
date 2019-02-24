package sensala.parser.german

import cats.Monad
import cats.implicits._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.coref.CorefCoreAnnotations
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.semgraph.{SemanticGraph, SemanticGraphCoreAnnotations}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap
import sensala.parser.DiscourseParser
import sensala.parser.german.GermanSensalaGrammaticalRelations._
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.adverb._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.noun._
import sensala.structure.noun.pronoun._
import sensala.structure.prepositional._
import sensala.structure.verb._
import sensala.structure.wh._

import scala.collection.convert.ImplicitConversionsToScala._

final case class GermanDiscourseParser[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError]()
    extends DiscourseParser[F] {
  private val logger = Logger[this.type]

  type EitherS[T] = Either[String, T]

  private def pairToTuple[U, V](p: edu.stanford.nlp.util.Pair[U, V]): (U, V) =
    (p.first, p.second)

  sealed trait CommonNounDeterminer
  case object Existential extends CommonNounDeterminer
  case object Forall      extends CommonNounDeterminer
  case object The         extends CommonNounDeterminer

  private val indefiniteArticles = Set(
    "ein",
    "ein",
    "eine",
    "einen",
    "ein",
    "eine",
    "einem",
    "einem",
    "einer",
    "eines",
    "eines",
    "einer"
  )

  private val definiteArticles = Set(
    "der",
    "das",
    "die",
    "den",
    "das",
    "die",
    "dem",
    "dem",
    "der",
    "des",
    "des",
    "der"
  )

  private val toBeVerbs = Set(
    "bin",
    "bist",
    "ist",
    "sind",
    "seid",
    "sind",
    "war",
    "warst",
    "war",
    "waren",
    "wart",
    "waren"
  )

  private def parseCommonNoun(
    nounPhrase: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, CommonNounDeterminer] = {
    require(nounPhrase.tag == "NN")
    val children = graph.childPairs(nounPhrase).toList.map(pairToTuple)
    val determiners = children.collect {
      case (rel, word) if rel == Det => word
    }
    determiners match {
      case Nil =>
        // FIXME: Currently treating all common nouns without determiners as indefinite
        Right(Existential)
      case x :: Nil if indefiniteArticles.contains(x.word.toLowerCase) =>
        Right(Existential)
      case x :: Nil if x.word.toLowerCase == "jeder" =>
        Right(Forall)
      case x :: Nil if definiteArticles.contains(x.word.toLowerCase) =>
        Right(The)
      case x :: Nil =>
        Left(s"Unknown determiner: ${x.word}")
      case _ =>
        Left(s"Multiple determiners: ${determiners.map(_.word).mkString(" ")}")
    }
  }

  private def parseAdjectiveNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase[F]
  )(implicit graph: SemanticGraph): Either[String, NounPhrase[F]] = {
    val modifiers = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdjMod => word
    }
    for {
      mods <- modifiers.map {
               case x if x.tag == "JJ" => Right(Adjective(x.word))
               case x                  => Left(s"Unknown modifier: (${x.tag}) ${x.word}")
             }.sequence[EitherS, Adjective]
    } yield mods.foldRight(nounPhrase)((adj, np) => AdjectiveNounPhrase(adj, np))
  }

  private def parseWhNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase[F]
  )(implicit graph: SemanticGraph): Either[String, NounPhrase[F]] = {
    val clauses = graph
      .childPairs(nounTree)
      .toList
      .map(pairToTuple)
      .collect {
        case (clRel, clWord) if clRel == ClMod =>
          graph.childPairs(clWord).toList.map(pairToTuple).collectFirst {
            case (rel, word) if word.tag == "PRELS" && rel == NSubj =>
              (word, clWord)
          }
      }
      .flatten
    for {
      whClauses <- clauses.collect {
                    case (ref, relClause) if ref.word.toLowerCase == "der" =>
                      parseVerbPhrase(relClause)
                  }.sequence[EitherS, VerbPhrase[F]]
    } yield whClauses.foldRight(nounPhrase)(WhNounPhrase.apply)
  }

  private def parseAdverbVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase[F]
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase[F]] = {
    val adverbs = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdvMod => word
    }
    val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
    Right(adverbMods.foldRight(verbPhrase)(VerbAdverbPhrase.apply))
  }

  private def parsePrepositionalVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase[F]
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase[F]] = {
    val prepositions = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if NomMod.isAncestor(rel) => word
    }
    for {
      prepositionModifiers <- prepositions.map { preposition =>
                               for {
                                 prepositionNounPhrase <- parseNounPhrase(preposition)
                                 prepositionGraphMap = graph
                                   .childPairs(preposition)
                                   .map(pairToTuple)
                                   .toMap
                                 caseWord <- prepositionGraphMap
                                              .get(Case)
                                              .toRight("Invalid preposition: no case word")
                               } yield InPhrase(caseWord.word, prepositionNounPhrase)
                             }.sequence[EitherS, InPhrase[F]]
    } yield prepositionModifiers.foldRight(verbPhrase)(VerbInPhrase.apply)
  }

  private def parseVerbPhrase(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase[F]] =
    verbTree.tag match {
      case "VVFIN" | "VVIMP" | "VVINF" | "VVIZU" | "VVPP" =>
        val childrenMap         = graph.childPairs(verbTree).map(pairToTuple).toMap
        val objOpt              = childrenMap.get(DObj)
        val clausalComponentOpt = childrenMap.get(CComp)
        (objOpt, clausalComponentOpt) match {
          case (Some(_), Some(_)) =>
            Left("Illegal verb phrase: object and clausal component cannot be specified together")
          case (Some(obj), None) =>
            for {
              objPhrase <- parseNounPhrase(obj)
              adverbVerbPhrase <- parseAdverbVerbPhrase(
                                   verbTree,
                                   TransitiveVerb(verbTree.word, objPhrase)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
          case (None, Some(clausalComponent)) =>
            for {
              clausalSentence <- parseSentence(clausalComponent)
              adverbVerbPhrase <- parseAdverbVerbPhrase(
                                   verbTree,
                                   VerbSentencePhrase(verbTree.word, clausalSentence)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
          case (None, None) =>
            for {
              adverbVerbPhrase        <- parseAdverbVerbPhrase(verbTree, IntransitiveVerb(verbTree.word))
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
        }
      case _ =>
        Left("Invalid verb phrase")
    }

  private def parseVerbPhrasePassive(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase[F]] =
    verbTree.tag match {
      case "VVFIN" | "VVIMP" | "VVINF" | "VVIZU" | "VVPP" =>
        val childrenMap = graph.childPairs(verbTree).map(pairToTuple).toMap
        val passSubjOpt = childrenMap.get(NSubjPass)
        passSubjOpt match {
          case Some(passSubj) =>
            for {
              passSubjPhrase <- parseNounPhrase(passSubj)
            } yield TransitiveVerb(verbTree.word, passSubjPhrase)
          case None =>
            Right(IntransitiveVerb(verbTree.word))
        }
      case _ =>
        Left("Invalid passive verb phrase")
    }

  private def parsePrepositionalNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase[F]
  )(implicit graph: SemanticGraph): Either[String, NounPhrase[F]] = {
    val prepositions = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if NomMod.isAncestor(rel) || rel == NomModPoss => (rel, word)
    }
    for {
      prepositionModifiers <- prepositions.map {
                               case (rel, preposition) if rel == NomModOn =>
                                 for {
                                   prepositionNounPhrase <- parseNounPhrase(preposition)
                                   prepositionGraphMap = graph
                                     .childPairs(preposition)
                                     .map(pairToTuple)
                                     .toMap
                                   caseWord <- prepositionGraphMap
                                                .get(Case)
                                                .toRight("Invalid preposition: no case word")
                                 } yield InPhrase(caseWord.word, prepositionNounPhrase)
                               case (rel, preposition) if rel == NomModPoss =>
                                 for {
                                   prepositionNounPhrase <- parseNounPhrase(preposition)
                                 } yield PossessionPhrase(prepositionNounPhrase)
                               case _ =>
                                 Left("Illegal nominal modifier")
                             }.sequence[EitherS, PrepositionalPhrase[F]]
    } yield prepositionModifiers.foldRight(nounPhrase)(NounPhrasePreposition.apply)
  }

  private def parsePersonalPronoun(word: IndexedWord): Either[String, PersonalPronoun[F]] =
    word.word.toLowerCase match {
      case "ich" | "meiner" => Right(FirstPersonSingularPersonalPronoun(word.word))
      case "du" | "deiner"  => Right(SecondPersonSingularPersonalPronoun(word.word))
      case "er" | "ihn" | "ihm" | "seiner" =>
        Right(ThirdPersonSingularPersonalPronoun(word.word, Masculine))
      case "sie" | "ihr" | "ihrer"   => Right(ThirdPersonSingularPersonalPronoun(word.word, Feminine))
      case "es"                      => Right(ThirdPersonSingularPersonalPronoun(word.word, Neuter))
      case "wir" | "unser"           => Right(FirstPersonPluralPersonalPronoun(word.word))
      case "ihr" | "euer"            => Right(SecondPersonPluralPersonalPronoun(word.word))
      case "sie" | "ihnen" | "ihrer" => Right(ThirdPersonPluralPersonalPronoun(word.word))
      case _                         => Left(s"Unknown personal pronoun: ${word.word}")
    }

  private def parseReflexivePronoun(word: IndexedWord): Either[String, ReflexivePronoun[F]] =
    word.word.toLowerCase match {
      case "mich" | "mir" => Right(FirstPersonSingularReflexivePronoun(word.word))
      case "dich" | "dir" => Right(SecondPersonSingularReflexivePronoun(word.word))
      case "sich"         => Right(ThirdPersonSingularReflexivePronoun(word.word, Neuter))
      case "uns"          => Right(FirstPersonPluralReflexivePronoun(word.word))
      case "euch"         => Right(SecondPersonPluralReflexivePronoun(word.word))
      case "sich"         => Right(ThirdPersonPluralReflexivePronoun(word.word))
      case _              => Left(s"Unknown reflexive pronoun: ${word.word}")
    }

  private def parsePersonalOrReflexivePronoun(word: IndexedWord): Either[String, Pronoun[F]] =
    parsePersonalPronoun(word).orElse(parseReflexivePronoun(word))

  private def parsePossessivePronoun(word: IndexedWord): Either[String, PossessivePronoun[F]] =
    word.word.toLowerCase match {
      case _ => Left(s"Unknown possessive pronoun: ${word.word}")
    }

  private def parseNounPhrase(
    nounTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, NounPhrase[F]] =
    nounTree.tag match {
      case "NN" =>
        parseCommonNoun(nounTree) match {
          case Right(Existential) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      ExistentialQuantifier(CommonNoun(nounTree.word))
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Right(Forall) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      ForallQuantifier(CommonNoun(nounTree.word))
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Right(The) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(
                                      nounTree,
                                      DefiniteNounPhrase(CommonNoun(nounTree.word))
                                    )
              whNounPhrase   <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Left(error) =>
            Left(error)
        }
      case "NE" =>
        val ner = Option(nounTree.ner()).flatMap(parseNer)
        val gender =
          Option(nounTree.get(classOf[CoreAnnotations.GenderAnnotation])).flatMap(parseGender)
        val properNoun = ExistentialQuantifier(ProperNoun[F](nounTree.word, ner, gender))
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, properNoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "PPER" =>
        logger.info(
          Option(nounTree.backingLabel().get(classOf[CorefCoreAnnotations.CorefClusterAnnotation]))
            .map(_.toSet)
            .toString
        )
        for {
          pronoun             <- parsePersonalOrReflexivePronoun(nounTree)
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, pronoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "PPOSS" =>
        for {
          possessivePronoun   <- parsePossessivePronoun(nounTree)
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, possessivePronoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case _ => Left(s"Unknown noun phrase: (${nounTree.tag}) ${nounTree.word}")
    }

  private def parseSentence(
    root: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, Sentence[F]] =
    root.tag match {
      case "VVFIN" | "VVIMP" | "VVINF" | "VVIZU" | "VVPP" =>
        val children         = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap      = children.toMap
        val subjOpt          = childrenMap.get(NSubj)
        val verbPhraseEither = parseVerbPhrase(root)
        (subjOpt, verbPhraseEither) match {
          case (Some(subj), Right(verbPhrase)) =>
            for {
              subjPhrase <- parseNounPhrase(subj)
            } yield Sentence(subjPhrase, verbPhrase)
          case (None, _) =>
            val agents = children.collect {
              case (rel, word) if NomMod.isAncestor(rel) && rel.getSpecific == "agent" => word
            }
            val auxPassOpt = childrenMap.get(AuxPass)
            (agents, auxPassOpt) match {
              case (Nil, _) =>
                Left("Illegal sentence: no subject and no agent")
              case (agentWord :: Nil, Some(verbWord))
                  if toBeVerbs.contains(verbWord.word.toLowerCase) =>
                for {
                  agentPhrase <- parseNounPhrase(agentWord)
                  verbPhrase  <- parseVerbPhrasePassive(root)
                } yield Sentence(agentPhrase, verbPhrase)
              case (_, None) =>
                Left("Illegal passive voice sentence: no auxiliary verb")
              case (_, _) =>
                Left("Illegal sentence: multiple agents")
            }
          case (_, Left(error)) =>
            Left(error)
        }
      case "JJ" =>
        val childrenMap = graph.childPairs(root).map(pairToTuple).toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt      = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            if (toBeVerbs.contains(cop.word.toLowerCase)) {
              for {
                subjPhrase              <- parseNounPhrase(subj)
                verbPhrase              = VerbAdjectivePhrase[F](cop.word, Adjective(root.word))
                adverbVerbPhrase        <- parseAdverbVerbPhrase(root, verbPhrase)
                prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(root, adverbVerbPhrase)
              } yield Sentence(subjPhrase, prepositionalVerbPhrase)
            } else {
              Left(s"Invalid copular verb: ${cop.word}")
            }
          case (None, _) =>
            Left("Illegal sentence: no copular verb")
          case (_, None) =>
            Left("Illegal sentence: no subject")
        }
      case "NN" | "NE" | "PPER" =>
        val children    = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap = children.toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt      = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            if (toBeVerbs.contains(cop.word.toLowerCase)) {
              for {
                subjPhrase <- parseNounPhrase(subj)
                objPhrase  <- parseNounPhrase(root)
              } yield Sentence(subjPhrase, TransitiveVerb(cop.word, objPhrase))
            } else {
              Left(s"Invalid copular verb: ${cop.word}")
            }
          case (None, _) =>
            Left("Illegal sentence: no copular verb")
          case (_, None) =>
            Left("Illegal sentence: no subject")
        }
    }

  private def transformVerbPhraseAnaphora(sentence: Sentence[F]): Sentence[F] =
    sentence.verbPhrase match {
      case _ =>
        sentence
    }

  private def parseSentence(sentence: CoreMap): Either[String, Sentence[F]] = {
    implicit val graph =
      sentence.get(classOf[SemanticGraphCoreAnnotations.EnhancedPlusPlusDependenciesAnnotation])
    logger.info("\n" + graph.toString)
    val root = graph.getFirstRoot
    parseSentence(root).map(transformVerbPhraseAnaphora)
  }

  def buildPennTaggedTree(discourse: String): List[Tree] = {
    val document = new Annotation(discourse)
    GermanSensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    sentences.map(_.get(classOf[TreeAnnotation]))
  }

  def parse(discourse: String): Either[String, Discourse[F]] = {
    val document = new Annotation(discourse)
    GermanSensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    for {
      result <- sentences
                 .map(parseSentence)
                 .sequence
    } yield Discourse(result)
  }
}
