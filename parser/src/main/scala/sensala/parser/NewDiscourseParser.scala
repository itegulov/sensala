package sensala.parser

import edu.stanford.nlp.ling.IndexedWord
import edu.stanford.nlp.semgraph.SemanticGraph
import edu.stanford.nlp.simple._
import cats.implicits._
import sensala.structure.Discourse
import sensala.structure.adjective._
import sensala.structure.adverb.{Adverb, VerbAdverbPhrase}
import sensala.structure.noun._
import sensala.structure.prepositional.InPhrase
import sensala.structure.verb._
import sensala.structure.wh._
import sensala.parser.SensalaGrammaticalRelations._

import scala.collection.convert.ImplicitConversionsToScala._

object NewDiscourseParser {
  type EitherS[T] = Either[String, T]
  
  private def pairToTuple[U, V](p: edu.stanford.nlp.util.Pair[U, V]): (U, V) =
    (p.first, p.second)

  sealed trait CommonNounDeterminer
  case object Existential extends CommonNounDeterminer
  case object Forall      extends CommonNounDeterminer

  def parseCommonNoun(
    nounPhrase: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, CommonNounDeterminer] = {
    require(nounPhrase.tag == "NN")
    val children = graph.childPairs(nounPhrase).toList.map(pairToTuple)
    val determiners = children.collect {
      case (rel, word) if rel == Det => word
    }
    determiners match {
      case x :: Nil if x.word.toLowerCase == "a" || x.word.toLowerCase == "an" =>
        Right(Existential)
      case x :: Nil if x.word.toLowerCase == "every" =>
        Right(Forall)
      case x :: Nil =>
        Left(s"Unknown determiner: ${x.word}")
      case _ =>
        Left(s"Multiple determiners: ${determiners.map(_.word).mkString(" ")}")
    }
  }
  
  def parseAdjectiveNounPhrase(
    nounTree: IndexedWord,
    nounPhrase: NounPhrase
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] = {
    val modifiers = graph.childPairs(nounTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdjMod => word
    }
    for {
      mods <- modifiers.map {
        case x if x.tag == "JJ" => Right(Adjective(x.word))
        case x => Left(s"Unknown modifier: (${x.tag}) ${x.word}")
      }.sequence[EitherS, Adjective]
    } yield mods.foldRight(nounPhrase)((adj, np) => AdjectiveNounPhrase(adj, np))
  }
  
  def parseWhNounPhrase(
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
  
  def parseAdverbVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] = {
    val adverbs = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if rel == AdvMod => word
    }
    val adverbMods = adverbs.map(indexedWord => Adverb(indexedWord.word))
    Right(adverbMods.foldRight(verbPhrase)(VerbAdverbPhrase.apply))
  }
  
  def parsePropositionalVerbPhrase(
    verbTree: IndexedWord,
    verbPhrase: VerbPhrase
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] = {
    val propositions = graph.childPairs(verbTree).toList.map(pairToTuple).collect {
      case (rel, word) if NomMod.isAncestor(rel) => word
    }
    for {
      propositionModifiers <- propositions.map { proposition =>
        for {
          propositionNounPhrase <- parseNounPhrase(proposition)
          propositionGraphMap = graph.childPairs(proposition).map(pairToTuple).toMap
          caseWord <- propositionGraphMap.get(Case).toRight("Invalid proposition: no case word")
        } yield InPhrase(caseWord.word, propositionNounPhrase)
      }.sequence[EitherS, InPhrase]
    } yield propositionModifiers.foldRight(verbPhrase)(VerbInPhrase.apply)
  }
  
  def parseVerbPhrase(
    verbTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, VerbPhrase] = {
    verbTree.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap = graph.childPairs(verbTree).map(pairToTuple).toMap
        val objOpt      = childrenMap.get(DObj)
        val clausalComponentOpt = childrenMap.get(CComp)
        (objOpt, clausalComponentOpt) match {
          case (Some(obj), None) =>
            for {
              objPhrase <- parseNounPhrase(obj)
              adverbVerbPhrase <- parseAdverbVerbPhrase(verbTree, TransitiveVerb(verbTree.word, objPhrase))
              propositionalVerbPhrase <- parsePropositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield propositionalVerbPhrase
          case (None, Some(clausalComponent)) =>
            for {
              clausalSentence <- parseSentence(clausalComponent)
              adverbVerbPhrase <- parseAdverbVerbPhrase(verbTree, VerbSentencePhrase(verbTree.word, clausalSentence))
              propositionalVerbPhrase <- parsePropositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield propositionalVerbPhrase
          case (None, None) =>
            for {
              adverbVerbPhrase <- parseAdverbVerbPhrase(verbTree, IntransitiveVerb(verbTree.word))
              propositionalVerbPhrase <- parsePropositionalVerbPhrase(verbTree, adverbVerbPhrase)
            } yield propositionalVerbPhrase
        }
      case _ =>
        Left("Invalid verb phrase")
    }
  }

  def parseNounPhrase(
    nounTree: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, NounPhrase] = {
    nounTree.tag match {
      case "NN" =>
        parseCommonNoun(nounTree) match {
          case Right(Existential) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, CommonNoun(nounTree.word))
              whNounPhrase <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
            } yield ExistentialQuantifier(whNounPhrase)
          case Right(Forall) =>
            for {
              adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, CommonNoun(nounTree.word))
              whNounPhrase <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
            } yield ForallQuantifier(whNounPhrase)
          case Left(error) =>
            Left(error)
        }
      case "NNP" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, ProperNoun(nounTree.word))
          whNounPhrase <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
        } yield ExistentialQuantifier(whNounPhrase)
      case "PRP" =>
        for {
          adjectiveNounPhrase <- parseAdjectiveNounPhrase(nounTree, ReflexivePronoun(nounTree.word))
          whNounPhrase <- parseWhNounPhrase(nounTree, adjectiveNounPhrase)
        } yield whNounPhrase
      case _ => Left(s"Unknown noun phrase: (${nounTree.tag}) ${nounTree.word}")
    }
  }
  
  def parseSentence(
    root: IndexedWord
  )(implicit graph: SemanticGraph): Either[String, NounPhraseWithVerbPhrase] = {
    root.tag match {
      case "VB" | "VBZ" | "VBP" | "VBD" | "VBN" | "VBG" =>
        val childrenMap = graph.childPairs(root).map(pairToTuple).toMap
        val subjOpt     = childrenMap.get(NSubj)
        val verbPhraseEither  = parseVerbPhrase(root)
        (subjOpt, verbPhraseEither) match {
          case (Some(subj), Right(verbPhrase)) =>
            for {
              subjPhrase <- parseNounPhrase(subj)
            } yield NounPhraseWithVerbPhrase(subjPhrase, verbPhrase)
          case (_, Left(error)) => Left(error)
          case (None, _) =>
            Left("Illegal sentence: no subject")
        }
      case "JJ" =>
        val childrenMap = graph.childPairs(root).map(pairToTuple).toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt     = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            if (cop.word.toLowerCase == "am" || cop.word.toLowerCase == "are" || cop.word.toLowerCase == "is") {
              for {
                subjPhrase <- parseNounPhrase(subj)
              } yield NounPhraseWithVerbPhrase(subjPhrase, VerbAdjectivePhrase(cop.word, Adjective(root.word)))
            } else {
              Left(s"Invalid copular verb: ${cop.word}")
            }
          case (None, _) =>
            Left("Illegal sentence: no copular verb")
          case (_, None) =>
            Left("Illegal sentence: no subject")
        }
      case "NN" | "NNP" | "PRP" =>
        val childrenMap = graph.childPairs(root).map(pairToTuple).toMap
        val subjOpt     = childrenMap.get(NSubj)
        val copOpt     = childrenMap.get(Cop)
        (copOpt, subjOpt) match {
          case (Some(cop), Some(subj)) =>
            if (cop.word.toLowerCase == "am" || cop.word.toLowerCase == "are" || cop.word.toLowerCase == "is") {
              for {
                subjPhrase <- parseNounPhrase(subj)
                objPhrase <- parseNounPhrase(root)
              } yield NounPhraseWithVerbPhrase(subjPhrase, TransitiveVerb(cop.word, objPhrase))
            } else {
              Left(s"Invalid copular verb: ${cop.word}")
            }
          case (None, _) =>
            Left("Illegal sentence: no copular verb")
          case (_, None) =>
            Left("Illegal sentence: no subject")
        }
    }
  }
  
  def parseSentence(sentence: Sentence): Either[String, NounPhraseWithVerbPhrase] = {
    implicit val graph = sentence.dependencyGraph()
    val root           = graph.getFirstRoot
    parseSentence(root)
  }

  def parse(discourse: String): Either[String, Discourse] = {
    val document = new Document(discourse)
    for {
      result <- document.sentences.toList
        .map(parseSentence)
        .sequence
    } yield Discourse(result)
  }

  def main(args: Array[String]): Unit =
    println(parse("John is a farmer"))
}
