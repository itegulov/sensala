package au.edu.anu.sensala.parser

import scala.collection.JavaConverters._
import java.util.Properties

import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.trees.Tree
import au.edu.anu.sensala.structure._
import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
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
  
  def extractNounPhrase(tree: Tree): NounPhrase = {
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
          case (Some(nounPhrase), Some(subordinated)) => subordinated(nounPhrase)
          case _ =>
            val existentialDeterminers = tree.children.flatMap {
              child => child.label.value match {
                case "DT" if child.getChild(0).label.value.toLowerCase == "a" => Some(ExistentialQuantifier.apply _)
                case _ => None
              }
            }
            val forallDeterminers = tree.children.flatMap {
              child => child.label.value match {
                case "DT" if child.getChild(0).label.value.toLowerCase == "every" => Some(ForallQuantifier.apply _)
                case _ => None
              }
            }
            val nounWords = tree.children.flatMap {
              child => child.label.value match {
                case "NN" => Some(CommonNoun(child.getChild(0).label.value))
                case "NNP" => Some(ProperNoun(child.getChild(0).label.value))
                case "PRP" => Some(ReflexivePronoun(child.getChild(0).label.value))
                case _ => None
              }
            }
            val nounWordOpt = nounWords.headOption
            val existentialDeterminerOpt = existentialDeterminers.headOption
            val forallDeterminerOpt = forallDeterminers.headOption
            (existentialDeterminerOpt, forallDeterminerOpt, nounWordOpt) match {
              case (Some(existentialDet), None, Some(commonNoun: CommonNoun)) => existentialDet(commonNoun)
              case (Some(_), None, Some(_)) => sys.error("Existential determiner should be applied to common nouns")
              case (None, Some(forallDet), Some(commonNoun: CommonNoun)) => forallDet(commonNoun)
              case (None, Some(_), Some(_)) => sys.error("Forall determiner should be applied to common nouns")
              case (Some(_), Some(_), _) => sys.error("Forall determiner and existential determiner cannot be applied together")
              case (None, None, Some(_: CommonNoun)) => sys.error("Common noun should be used with a determiner")
              case (None, None, Some(nounWord: ProperNoun)) => nounWord
              case (None, None, Some(rf: ReflexivePronoun)) => rf
              case _ => sys.error("Invalid noun phrase")
            }
        }
    }
  }

  def extractVerbPhrase(tree: Tree): VerbPhrase = {
    tree.label.value match {
      case "VP" if tree.children.length == 1 =>
        IntransitiveVerb(tree.getChild(0).getChild(0).label.value)
      case "VP" if tree.children.length == 2 =>
        val transitiveVerb = TransitiveVerb(tree.getChild(0).getChild(0).label.value)
        VerbObjPhrase(transitiveVerb, extractNounPhrase(tree.getChild(1)))
    }
  }

  def convertSentence(tree: Tree): Sentence = {
    assert(tree.label.value == "ROOT")
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

  def parse(text: String): NL = {
    val document = new Annotation(text)
    pipeline.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[CoreAnnotations.SentencesAnnotation]).asScala.toList
    Discourse(sentences.map(sentence => convertSentence(sentence.get(classOf[TreeAnnotation]))))
  }
}
