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
  
  def extractAdjectiveNounPhrase(tree: Tree): NounPhrase = {
    tree.label.value match {
      case "NP" =>
        val embeddedNounPhrases = tree.children.flatMap {
          child => child.label.value match {
            case "NP" => Some(extractNounPhrase(child))
            case _ => None
          }
        }
        val subordinatedSentences = tree.children.flatMap {
          child => child.label.value match {
            case "SBAR" => Some(extractSubordinatedSentence(child))
            case _ => None
          }
        }
        
        (embeddedNounPhrases.headOption, subordinatedSentences.headOption) match {
          case (Some(ForallQuantifier(commonNoun)), Some(subordinated)) => ForallQuantifier(subordinated(commonNoun))
          case (Some(ExistentialQuantifier(commonNoun)), Some(subordinated)) => ExistentialQuantifier(subordinated(commonNoun))
          case (Some(nounPhrase), Some(subordinated)) => subordinated(nounPhrase)
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
            (adjectiveWordOpt, nounWordOpt) match {
              case (None, Some(commonNoun: CommonNoun)) => commonNoun
              case (None, Some(properNoun: ProperNoun)) => properNoun
              case (None, Some(rf: ReflexivePronoun)) => rf
              case (Some(adjective), Some(nounPhrase)) => AdjectivePhrase(adjective, nounPhrase)
              case _ => sys.error("Invalid noun phrase")
            }
        }
    }
  }
  
  def extractDeterminedNounPhrase(tree: Tree): NounPhrase = {
    val existentialDeterminers = tree.children.flatMap {
      child => child.label.value match {
        case "DT" if child.getChild(0).label.value.toLowerCase == "a" => Some(ExistentialQuantifier.apply _)
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
    (existentialDeterminerOpt, forallDeterminerOpt) match {
      case (None, None) =>
        extractAdjectiveNounPhrase(tree)
      case (Some(existentialDet), None) =>
        existentialDet(extractAdjectiveNounPhrase(tree))
      case (None, Some(forallDet)) =>
        forallDet(extractAdjectiveNounPhrase(tree))
      case (Some(_), Some(_)) =>
        sys.error("Forall determiner and existential determiner cannot be applied together")
    }
  }
  
  def extractNounPhrase(tree: Tree): NounPhrase = {
    extractDeterminedNounPhrase(tree)
  }

  def extractVerbPhrase(tree: Tree): VerbPhrase = {
    tree.label.value match {
      case "VP" if tree.children.length == 1 =>
        IntransitiveVerb(tree.getChild(0).getChild(0).label.value)
      case "VP" if tree.children.length == 2 =>
        val transitiveVerb = TransitiveVerb(tree.getChild(0).getChild(0).label.value)
        convert(tree.getChild(1)) match {
          case nounPhrase: NounPhrase => VerbObjPhrase(transitiveVerb, nounPhrase)
          case adjective: Adjective => VerbAdjectivePhrase(transitiveVerb, adjective)
          case sentence: Sentence => VerbSentencePhrase(transitiveVerb, sentence)
        }
    }
  }
  
  def extractAdjective(tree: Tree): Adjective = {
    tree.label.value match {
      case "ADJP" =>
        val adjectiveWords = tree.children.flatMap {
          child => child.label.value match {
            case "JJ" => Some(Adjective(child.getChild(0).label.value))
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

  def convertSentence(tree: Tree): Sentence = {
    assert(tree.label.value == "ROOT" || tree.label.value == "SBAR")
    logger.debug(tree.pennString)
    val s = tree.getChild(0)
    assert(s.label.value == "S")
    val verbPhrase = s.children.find(_.label.value == "VP")
    val verbOpt = verbPhrase.map(extractVerbPhrase)
    val nounPhrase = s.children.find(_.label.value == "NP")
    val nounOpt = nounPhrase.map(extractNounPhrase)
    (nounOpt, verbOpt) match {
      case (Some(noun), Some(verb)) => Sentence(noun, verb)
      case (None, None)             => sys.error("Invalid sentence")
    }
  }
  
  def convert(tree: Tree): NL = {
    tree.label.value match {
      case "SBAR" => convertSentence(tree)
      case "NP" => extractNounPhrase(tree)
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
