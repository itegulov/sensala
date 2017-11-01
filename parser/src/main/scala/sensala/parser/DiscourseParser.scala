package sensala.parser

import scala.collection.JavaConverters._
import java.util.Properties

import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.trees.Tree
import sensala.structure._
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.TreeCoreAnnotations._
import edu.stanford.nlp.util.CoreMap

object DiscourseParser {
  private val logger = Logger[this.type]
  private val props = new Properties()
  props.put("annotators", "tokenize, ssplit, pos, lemma, ner, parse, dcoref")
  private val pipeline = new StanfordCoreNLP(props)
  
  def extractSubordinatedSentence(tree: Tree): NounPhrase => WhNounPhrase = {
    assert(tree.label.value == "SBAR")
    val whNounPhrases = tree.children.flatMap {
      child => child.label.value match {
        case "WHNP" if child.getChild(0).label.value == "WP" => Some(WhNounPhrase.apply _)
        case _ => None
      }
    }
    val verbSentences = tree.children.flatMap {
      child => child.label.value match {
        case "S" if child.getChild(0).label.value == "VP" => Some(extractVerbPhrase(child.getChild(0)))
        case _ => None
      }
    }
    (whNounPhrases.headOption, verbSentences.headOption) match {
      case (Some(whNoun), Some(verb)) => (x: NounPhrase) => whNoun(verb, x)
    }
  }
  
  def extractAdjectiveNounPhrase(tree: Tree, verbOpt: Option[VerbPhrase]): NounPhrase = {
    tree.label.value match {
      case "NP" =>
        val embeddedNounPhrases = tree.children.flatMap {
          child => child.label.value match {
            case "NP" => Some(extractNounPhrase(child, None))
            case _ => None
          }
        }
        val subordinatedSentences = tree.children.flatMap {
          child => child.label.value match {
            case "SBAR" => Some(extractSubordinatedSentence(child))
            case _ => None
          }
        }
        
        (embeddedNounPhrases.headOption, subordinatedSentences.headOption, verbOpt) match {
          case (Some(ForallQuantifier(np)), Some(subordinated), None) => ForallQuantifier(subordinated(np))
          case (Some(ForallQuantifier(np)), Some(subordinated), Some(vp)) => ForallQuantifierVP(subordinated(np), vp)
          case (Some(ExistentialQuantifier(np)), Some(subordinated), None) => ExistentialQuantifier(subordinated(np))
          case (Some(ExistentialQuantifier(np)), Some(subordinated), Some(vp)) => ExistentialQuantifierVP(subordinated(np), vp)
          case (Some(np), Some(subordinated), None) => subordinated(np)
          case (Some(np), Some(subordinated), Some(vp)) => ???
          case _ =>
            val nounWords = tree.children.flatMap {
              child => child.label.value match {
                case "NN" => Some(CommonNoun(child.getChild(0).label.value))
                case "NNP" => Some(ProperNoun(child.getChild(0).label.value))
                case "PRP" => Some(ReflexivePronoun(child.getChild(0).label.value))
                case _ => None
              }
            }
            val adjectiveWords = tree.children.flatMap {
              child => child.label.value match {
                case "JJ" => Some(Adjective(child.getChild(0).label.value))
                case _ => None
              }
            }
            val nounWordOpt = nounWords.headOption
            val adjectiveWordOpt = adjectiveWords.headOption
            (adjectiveWordOpt, nounWordOpt, verbOpt) match {
              case (None, Some(commonNoun: CommonNoun), None) => commonNoun
              case (None, Some(_: CommonNoun), Some(_)) => sys.error("Verb phrase should not be applied to a raw (without a determiner) noun")
              case (None, Some(properNoun: ProperNoun), None) => properNoun
              case (None, Some(properNoun: ProperNoun), Some(vp)) => ProperNounVP(properNoun.word, vp)
              case (None, Some(rf: ReflexivePronoun), None) => rf
              case (None, Some(rf: ReflexivePronoun), Some(vp)) => ReflexivePronounVP(rf.word, vp)
              case (Some(adj), Some(np), None) => AdjectivePhrase(adj, np)
              case (Some(adj), Some(np), Some(vp)) => AdjectivePhraseVP(adj, np, vp)
              case _ => sys.error("Invalid noun phrase")
            }
        }
    }
  }
  
  def extractDeterminedNounPhrase(tree: Tree, verbOpt: Option[VerbPhrase]): NounPhrase = {
    val existentialDeterminers = tree.children.flatMap {
      child => child.label.value match {
        case "DT" if child.getChild(0).label.value.toLowerCase == "a"  => Some(ExistentialQuantifier.apply _)
        case "DT" if child.getChild(0).label.value.toLowerCase == "an"  => Some(ExistentialQuantifier.apply _)
        case "NNP" => Some(ExistentialQuantifier.apply _)
        case _ => None
      }
    }
    val forallDeterminers = tree.children.flatMap {
      child => child.label.value match {
        case "DT" if child.getChild(0).label.value.toLowerCase == "every" => Some(ForallQuantifier.apply _)
        case _ => None
      }
    }
    val existentialDeterminerOpt = existentialDeterminers.headOption
    val forallDeterminerOpt = forallDeterminers.headOption
    (existentialDeterminerOpt, forallDeterminerOpt, verbOpt) match {
      case (None, None, _) =>
        extractAdjectiveNounPhrase(tree, verbOpt)
      case (Some(_), None, None) =>
        ExistentialQuantifier(extractAdjectiveNounPhrase(tree, None))
      case (Some(_), None, Some(vp)) =>
        ExistentialQuantifierVP(extractAdjectiveNounPhrase(tree, None), vp)
      case (None, Some(forallDet), None) =>
        ForallQuantifier(extractAdjectiveNounPhrase(tree, None))
      case (None, Some(forallDet), Some(vp)) =>
        ForallQuantifierVP(extractAdjectiveNounPhrase(tree, None), vp)
      case (Some(_), Some(_), _) =>
        sys.error("Forall determiner and existential determiner cannot be applied together")
    }
  }
  
  def extractNounPhrase(tree: Tree, verbOpt: Option[VerbPhrase]): NounPhrase = {
    extractDeterminedNounPhrase(tree, verbOpt)
  }

  def extractVerbPhrase(tree: Tree): VerbPhrase = {
    tree.label.value match {
      case "VP" if tree.children.length == 1 =>
        IntransitiveVerb(tree.getChild(0).getChild(0).label.value)
      case "VP" if tree.children.length == 2 =>
        val transitiveVerb = tree.getChild(0).getChild(0).label.value
        convert(tree.getChild(1)) match {
          case nounPhrase: NounPhraseWithoutVerbPhrase => VerbObjPhrase(transitiveVerb, nounPhrase)
          case adjective: Adjective => VerbAdjectivePhrase(transitiveVerb, adjective)
          case sentence: NounPhraseWithVerbPhrase => VerbSentencePhrase(transitiveVerb, sentence)
        }
    }
  }
  
  def extractAdjective(tree: Tree): Adjective = {
    tree.label.value match {
      case "ADJP" =>
        val adjectiveWords = tree.children.flatMap {
          child => child.label.value match {
            case "JJ" => Some(Adjective(child.getChild(0).label.value))
            case "RB" => Some(Adjective(child.getChild(0).label.value)) // TODO: Adverb are generally not adjectives
            case _ => None
          }
        }
        val adjectiveWordOpt = adjectiveWords.headOption
        adjectiveWordOpt match {
          case Some(adjective) => adjective
          case None => sys.error("Invalid adjective phrase")
        }
    }
  }

  def convertSentence(tree: Tree): NounPhraseWithVerbPhrase = {
    assert(tree.label.value == "ROOT" || tree.label.value == "SBAR")
    logger.debug(tree.pennString)
    val s = tree.getChild(0)
    assert(s.label.value == "S")
    val verbPhrase = s.children.find(_.label.value == "VP")
    val verbOpt = verbPhrase.map(extractVerbPhrase)
    val nounPhrase = s.children.find(_.label.value == "NP")
    val nounOpt = nounPhrase.map(extractNounPhrase(_, verbOpt))
    nounOpt match {
      case Some(np: NounPhraseWithVerbPhrase)    => np
      case Some(_: NounPhraseWithoutVerbPhrase) => sys.error("Sentence should contain a verb phrase")
      case None => sys.error("Sentence should contain a noun phrase")
    }
  }
  
  def convert(tree: Tree): NL = {
    tree.label.value match {
      case "SBAR" => convertSentence(tree)
      case "NP" => extractNounPhrase(tree, None)
      case "ADJP" => extractAdjective(tree)
    }
  }

  def parse(text: String): NL = {
    val document = new Annotation(text)
    pipeline.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toList
    Discourse(sentences.map(sentence => convertSentence(sentence.get(classOf[TreeAnnotation]))))
  }
}
