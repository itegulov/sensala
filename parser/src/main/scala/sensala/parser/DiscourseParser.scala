package sensala.parser

import edu.stanford.nlp.ling.{CoreAnnotations, IndexedWord}
import edu.stanford.nlp.semgraph.{SemanticGraph, SemanticGraphCoreAnnotations}
import cats.implicits._
import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.process.Morphology
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap
import sensala.structure._
import sensala.structure.Sentence
import sensala.structure.adjective._
import sensala.structure.adverb._
import sensala.structure.noun._
import sensala.structure.prepositional._
import sensala.structure.verb._
import sensala.structure.wh._
import sensala.parser.SensalaGrammaticalRelations._

import scala.collection.convert.ImplicitConversionsToScala._

object DiscourseParser {
  type EitherS[T] = Either[String, T]

  private def pairToTuple[U, V](p: edu.stanford.nlp.util.Pair[U, V]): (U, V) =
    (p.first, p.second)

  sealed trait CommonNounDeterminer
  case object Existential extends CommonNounDeterminer
  case object Forall      extends CommonNounDeterminer
  case object The         extends CommonNounDeterminer

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
      case x :: Nil if x.word.toLowerCase == "a" || x.word.toLowerCase == "an" =>
        Right(Existential)
      case x :: Nil if x.word.toLowerCase == "every" =>
        Right(Forall)
      case x :: Nil if x.word.toLowerCase == "the" =>
        Right(The)
      case x :: Nil =>
        Left(s"Unknown determiner: ${x.word}")
      case _ =>
        Left(s"Multiple determiners: ${determiners.map(_.word).mkString(" ")}")
    }
  }

  private def parseAdjectiveNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] = {
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
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] = {
    val refs = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == Ref => word
    }
    val relativeClauses = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == RelClMod => word
    }
    // FIXME: References and relative clauses can be ordered differently
    val clauses = refs.zip(relativeClauses)
    for {
      whClauses <- clauses.collect {
                    case (ref, relClause) if ref.word.toLowerCase == "who" =>
                      parseVerbPhrase(relClause)
                  }.sequence[EitherS, VerbPhrase]
    } yield whClauses.foldRight(nounPhrase)(WhNounPhrase.apply)
  }

  private def parseAdverbVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] = {
    val adverbs = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdvMod => word
    }
    val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
    Right(adverbMods.foldRight(verbPhrase)(VerbAdverbPhrase.apply))
  }

  private def parsePrepositionalVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] = {
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
                             }.sequence[EitherS, InPhrase]
    } yield prepositionModifiers.foldRight(verbPhrase)(VerbInPhrase.apply)
  }

  private def parseVerbPhrase(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] =
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
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
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] =
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
            Right(IntransitiveVerb(verbTree.word))
        }
      case _ =>
        Left("Invalid passive verb phrase")
    }

  private def parsePrepositionalNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] = {
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
                             }.sequence[EitherS, PrepositionalPhrase]
    } yield prepositionModifiers.foldRight(nounPhrase)(NounPhrasePreposition.apply)
  }
  
  private def parseNer(nerString: String): Option[NamedEntityType] = nerString match {
    case "LOCATION"     => Some(Person)
    case "PERSON"       => Some(Person)
    case "ORGANIZATION" => Some(Organization)
    case "MONEY"        => Some(Money)
    case "PERCENT"      => Some(Percent)
    case "DATE"         => Some(Date)
    case "TIME"         => Some(Time)
    case _              => None
  }
  
  private def parseGender(genderString: String): Option[NamedEntityGender] = genderString match {
    case "MALE"   => Some(Male)
    case "FEMALE" => Some(Female)
    case _        => None
  }

  private def parseNounPhrase(
    nounTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] =
    nounTree.tag match {
      case "NN" =>
        parseCommonNoun(nounTree) match {
          case Right(Existential) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, ExistentialQuantifier(CommonNoun(nounTree.word)))
              whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Right(Forall) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, ForallQuantifier(CommonNoun(nounTree.word)))
              whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Right(The) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, DefiniteNounPhrase(CommonNoun(nounTree.word)))
              whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
              prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
            } yield prepNounPhrase
          case Left(error) =>
            Left(error)
        }
      case "NNP" =>
        val ner = Option(nounTree.ner()).flatMap(parseNer)
        val gender = Option(nounTree.get(classOf[CoreAnnotations.GenderAnnotation])).flatMap(parseGender)
        val properNoun = ExistentialQuantifier(ProperNoun(nounTree.word, ner, gender))
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, properNoun)
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "PRP" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, ReflexivePronoun(nounTree.word))
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case "PRP$" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, PossessivePronoun(nounTree.word))
          whNounPhrase        <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
          prepNounPhrase      <- parsePrepositionalNounPhrase(nounTree, whNounPhrase)
        } yield prepNounPhrase
      case _ => Left(s"Unknown noun phrase: (${nounTree.tag}) ${nounTree.word}")
    }

  private def parseSentence(
    root: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, Sentence] =
    root.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
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
              case (agent :: Nil, Some(verbWord))
                  if verbWord.word.toLowerCase == "am" || verbWord.word.toLowerCase == "is" || verbWord.word.toLowerCase == "are" =>
                for {
                  agentPhrase <- parseNounPhrase(agent)
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
            val stemCop = Morphology.stemStatic(cop.word, cop.tag)
            if (stemCop.word.toLowerCase == "be") {
              for {
                subjPhrase              <- parseNounPhrase(subj)
                verbPhrase              = VerbAdjectivePhrase(cop.word, Adjective(root.word))
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
              Left(s"Invalid copular verb: ${cop.word}")
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
                  } yield Sentence(objPhrase, VerbPhraseAnaphora(soWord.word + " " + cop.word))
                case None =>
                  Left("Invalid verb phrase anaphora")
              }
            } else {
              Left(s"Invalid copular verb: ${cop.word}")
            }
          case (None, _) =>
            Left("Illegal sentence: no copular verb")
          case (_, None) =>
            Left("Illegal sentence: no subject")
        }
    }

  private def transformVerbPhraseAnaphora(sentence: Sentence): Sentence =
    sentence.verbPhrase match {
      case VerbAdverbPhrase(Adverb("too"), IntransitiveVerb(word))
          if word.toLowerCase == "did" || word.toLowerCase == "does" =>
        Sentence(sentence.nounPhrase, VerbPhraseAnaphora(word + " too"))
      case VerbAdverbPhrase(Adverb(adWord), IntransitiveVerb(word))
          if adWord.toLowerCase == "so" || (word.toLowerCase == "did" || word.toLowerCase == "is") =>
        Sentence(sentence.nounPhrase, VerbPhraseAnaphora(adWord + " " + word))
      case _ =>
        sentence
    }
  
  private def parseSentence(sentence: CoreMap): Either[String, Sentence] = {
    implicit val graph = sentence.get(classOf[SemanticGraphCoreAnnotations.BasicDependenciesAnnotation])
    val root           = graph.getFirstRoot
    parseSentence(root).map(transformVerbPhraseAnaphora)
  }

  def buildPennTaggedTree(discourse: String): List[Tree] = {
    val document = new Annotation(discourse)
    SensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    sentences.map(_.get(classOf[TreeAnnotation]))
  }
  
  def parse(discourse: String): Either[String, Discourse] = {
    val document = new Annotation(discourse)
    SensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).toList
    for {
      result <- sentences
        .map(parseSentence)
        .sequence
    } yield Discourse(result)
  }
}
