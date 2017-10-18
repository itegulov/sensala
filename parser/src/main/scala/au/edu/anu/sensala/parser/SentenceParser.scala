package au.edu.anu.sensala.parser

import java.io.StringReader

import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.parser.lexparser.LexicalizedParser
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import edu.stanford.nlp.trees.Tree
import au.edu.anu.sensala.structure._

object SentenceParser {
  private val logger = Logger[this.type]
  private val parserModel = "edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz"
  private val lp = LexicalizedParser.loadModel(parserModel)

  def extractNounPhrase(tree: Tree): NounPhrase = {
    tree.label.value match {
      case "NP" =>
        val nounWords = tree.children.map {
          child => child.label.value match {
            case "NN" => CommonNoun(child.getChild(0).label.value)
            case "NNP" => ProperNoun(child.getChild(0).label.value)
            case "PRP" => ReflexivePronoun(child.getChild(0).label.value)
          }
        }
        nounWords.head
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

  def convert(tree: Tree): Sentence = {
    logger.debug(tree.pennString)
    val s = tree.getChild(0)
    val nounPhrase = s.children.find(_.label.value == "NP")
    val verbPhrase = s.children.find(_.label.value == "VP")
    (nounPhrase, verbPhrase) match {
      case (Some(noun), Some(verb)) =>
        Sentence(extractNounPhrase(noun), extractVerbPhrase(verb))
      case _ =>
        sys.error("Invalid sentence")
    }
  }

  def parse(text: String): Sentence = {
    val tokenizerFactory = PTBTokenizer.factory(new CoreLabelTokenFactory(), "")
    val tok = tokenizerFactory.getTokenizer(new StringReader(text))
    val words = tok.tokenize()
    val tree = lp.apply(words)
    convert(tree)
  }
}
