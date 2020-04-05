package sensala.parser.english

import cats.Monad
import cats.implicits._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.ie.NumberNormalizer
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.process.Morphology
import edu.stanford.nlp.semgraph.{SemanticGraph, SemanticGraphCoreAnnotations}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap
import sensala.parser.DiscourseParser
import sensala.parser.english.EnglishSensalaGrammaticalRelations._
import sensala.models.nl._
import sensala.parser.english.ParserError.HandleParserError

import scala.collection.convert.ImplicitConversionsToScala._
import scala.util.{Failure, Success, Try}

class EnglishDiscourseParser[F[_]: Monad: HandleParserError: PronounParser]
    extends DiscourseParser {
  private val logger = Logger[this.type]

  private def pairToTuple[U, V](p: edu.stanford.nlp.util.Pair[U, V]): (U, V) =
    (p.first, p.second)

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
        parseVerbPhrase(relClause).map(WhNounPhrase(_, prevNp))
      case (prevNp, (ref, relClause)) if ref.word.toLowerCase == "that" =>
        parseVerbPhrase(relClause).map(RelativeClausePhrase("that", _, prevNp))
    }
  }

  private def parseAdverbialClauseVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): F[VerbPhrase] = {
    val adverbialClauses = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if AdvclMod.isAncestor(rel) => (rel.getSpecific, word)
    }
    val adverbs = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdvMod => word
    }
    adverbialClauses match {
      case Nil =>
        val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
        adverbMods.foldRight(verbPhrase)(VerbAdverbPhrase.apply).pure[F]
      case (mark, adverbialClause) :: Nil =>
        val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
        for {
          clause <- parseSentence(adverbialClause)
        } yield VerbAdverbialClausePhrase(mark, clause, adverbMods, verbPhrase)
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Multiple adverbial clauses are unsupported"))
    }
  }

  private def parsePrepositionalVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): F[VerbPhrase] = {
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

  private def parseVerbPhrase(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): F[VerbPhrase] =
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap         = graph.childPairs(verbTree).map(pairToTuple).toMap
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
              objPhrase <- parseNounPhrase(obj)
              adverbVerbPhrase <- parseAdverbialClauseVerbPhrase(
                                   verbTree,
                                   TransitiveVerb(verbTree.word, objPhrase)
                                 )
              prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield prepositionalVerbPhrase
          case (None, Some(clausalComponent)) =>
            for {
              clausalSentence <- parseSentence(clausalComponent)
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

  private def parseVerbPhrasePassive(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): F[VerbPhrase] =
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap = graph.childPairs(verbTree).map(pairToTuple).toMap
        val passSubjOpt = childrenMap.get(NSubjPass)
        passSubjOpt match {
          case Some(passSubj) =>
            for {
              passSubjPhrase <- parseNounPhrase(passSubj)
            } yield TransitiveVerb(verbTree.word, passSubjPhrase)
          case None =>
            IntransitiveVerb(verbTree.word).pure[F].widen
        }
      case _ =>
        HandleParserError[F].raise(InvalidDiscourse("Invalid passive verb phrase"))
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

  private def parseNounPhrase(
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

  private def parseSentence(
    root: IndexedWord
  )(implicit graph: SemanticGraph): F[Sentence] =
    root.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val children    = graph.childPairs(root).map(pairToTuple).toList
        val childrenMap = children.toMap
        val subjOpt     = childrenMap.get(NSubj)
        for {
          verbPhraseEither <- HandleParserError[F].attempt(parseVerbPhrase(root))
          result <- (subjOpt, verbPhraseEither) match {
                     case (Some(subj), Right(verbPhrase)) =>
                       for {
                         subjPhrase <- parseNounPhrase(subj)
                       } yield Sentence(subjPhrase, verbPhrase)
                     case (None, _) =>
                       val agents = children.collect {
                         case (rel, word) if NomMod.isAncestor(rel) && rel.getSpecific == "agent" =>
                           word
                       }
                       val auxPassOpt = childrenMap.get(AuxPass)
                       (agents, auxPassOpt) match {
                         case (Nil, _) =>
                           HandleParserError[F].raise(
                             InvalidDiscourse("Illegal sentence: no subject and no agent")
                           )
                         case (agentWord :: Nil, Some(verbWord))
                             if verbWord.word.toLowerCase == "am" || verbWord.word.toLowerCase == "is" || verbWord.word.toLowerCase == "are" =>
                           for {
                             agentPhrase <- parseNounPhrase(agentWord)
                             verbPhrase  <- parseVerbPhrasePassive(root)
                           } yield Sentence(agentPhrase, verbPhrase)
                         case (_, None) =>
                           HandleParserError[F].raise(
                             InvalidDiscourse("Illegal passive voice sentence: no auxiliary verb")
                           )
                         case (_, _) =>
                           HandleParserError[F].raise(
                             InvalidDiscourse("Illegal sentence: multiple agents")
                           )
                       }
                     case (_, Left(error)) =>
                       HandleParserError[F].raise(error)
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
                subjPhrase              <- parseNounPhrase(subj)
                verbPhrase              = VerbAdjectivePhrase(cop.word, Adjective(root.word))
                adverbVerbPhrase        <- parseAdverbialClauseVerbPhrase(root, verbPhrase)
                prepositionalVerbPhrase <- parsePrepositionalVerbPhrase(root, adverbVerbPhrase)
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
                subjPhrase <- parseNounPhrase(subj)
                objPhrase  <- parseNounPhrase(root)
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
                    objPhrase <- parseNounPhrase(root)
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
                subjPhrase <- parseNounPhrase(subj)
                thanPhrase <- parseNounPhrase(thanClause)
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

  def buildPennTaggedTree(discourse: String): F[List[Tree]] = {
    val document = new Annotation(discourse)
    EnglishSensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    sentences.map(_.get(classOf[TreeAnnotation])).pure[F]
  }

  def parse(discourse: String): F[Discourse] = {
    val document = new Annotation(discourse)
    EnglishSensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    for {
      result <- sentences.map(parseSentence).sequence
    } yield Discourse(result)
  }
}

object EnglishDiscourseParser {
  def apply[F[_]](implicit ev: EnglishDiscourseParser[F]): EnglishDiscourseParser[F] = ev
}
