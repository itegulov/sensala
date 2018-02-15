package sensala.parser

import scala.collection.JavaConverters._
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.ling.CoreAnnotations._
import edu.stanford.nlp.pipeline.Annotation
import edu.stanford.nlp.trees.TreeCoreAnnotations._
import edu.stanford.nlp.util.CoreMap
import cats.implicits._
import sensala.property.PropertyExtractor
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.adverb.Adverb
import sensala.structure.noun._
import sensala.structure.prepositional.{InPhrase, PrepositionalPhrase}
import sensala.structure.verb._
import sensala.structure.wh._

case class DiscourseParser(implicit propertyExtractor: PropertyExtractor) {
  private val logger = Logger[this.type]
  type EitherS[T] = Either[String, T]

  def extractSubordinatedSentence(tree: Tree): Either[String, NounPhrase => WhNounPhrase] = {
    assert(tree.label.value == "SBAR")
    val whNounPhrases = tree.children.flatMap { child =>
      child.label.value match {
        case "WHNP" if child.getChild(0).label.value == "WP" =>
          Some(WhNounPhrase.apply _)
        case _ =>
          None
      }
    }
    val verbSentences = tree.children.flatMap { child =>
      child.label.value match {
        case "S" if child.getChild(0).label.value == "VP" =>
          Some(extractVerbPhrase(child.getChild(0)))
        case _ =>
          None
      }
    }
    (whNounPhrases.headOption, verbSentences.headOption) match {
      case (Some(whNoun), Some(Right(verb))) =>
        Right(x => whNoun(verb, x))
      case _ =>
        Left("Invalid wh-noun phrase")
    }
  }

  def extractAdjectiveNounPhrase(
    tree: Tree,
    verbOpt: Option[VerbPhrase]
  ): Either[String, NounPhrase] =
    tree.label.value match {
      case "NP" =>
        for {
          embeddedNounPhrases <- tree.children.flatMap { child =>
                                  child.label.value match {
                                    case "NP" => Some(extractNounPhrase(child, None))
                                    case _    => None
                                  }
                                }.toList.sequence[EitherS, NounPhrase]
          subordinatedSentences <- tree.children.flatMap { child =>
                                    child.label.value match {
                                      case "SBAR" => Some(extractSubordinatedSentence(child))
                                      case _      => None
                                    }
                                  }.toList.sequence[EitherS, NounPhrase => WhNounPhrase]
          result <- (embeddedNounPhrases.headOption, subordinatedSentences.headOption, verbOpt) match {
                     case (Some(ForallQuantifier(np)), Some(subordinated), None) =>
                       Right(ForallQuantifier(subordinated(np)))
                     case (Some(ForallQuantifier(np)), Some(subordinated), Some(vp)) =>
                       Right(ForallQuantifierVP(subordinated(np), vp))
                     case (Some(ExistentialQuantifier(np)), Some(subordinated), None) =>
                       Right(ExistentialQuantifier(subordinated(np)))
                     case (Some(ExistentialQuantifier(np)), Some(subordinated), Some(vp)) =>
                       Right(ExistentialQuantifierVP(subordinated(np), vp))
                     case (Some(np), Some(subordinated), None) =>
                       Right(subordinated(np))
                     case (Some(np), Some(subordinated), Some(_)) =>
                       Left("Subordinated sentence cannot have a verb phrase")
                     case _ =>
                       val nounWords = tree.children.flatMap { child =>
                         child.label.value match {
                           case "NN"  => Some(CommonNoun(child.getChild(0).label.value))
                           case "NNP" => Some(ProperNoun(child.getChild(0).label.value))
                           case "PRP" => Some(ReflexivePronoun(child.getChild(0).label.value))
                           case _     => None
                         }
                       }
                       val adjectiveWords = tree.children.flatMap { child =>
                         child.label.value match {
                           case "JJ" => Some(Adjective(child.getChild(0).label.value))
                           case _    => None
                         }
                       }
                       val nounWordOpt      = nounWords.headOption
                       val adjectiveWordOpt = adjectiveWords.headOption
                       (adjectiveWordOpt, nounWordOpt, verbOpt) match {
                         case (None, Some(commonNoun: CommonNoun), None) =>
                           Right(commonNoun)
                         case (None, Some(_: CommonNoun), Some(_)) =>
                           Left("Verb phrase should not be applied to a non-determined noun")
                         case (None, Some(properNoun: ProperNoun), None) =>
                           Right(properNoun)
                         case (None, Some(properNoun: ProperNoun), Some(vp)) =>
                           Right(ProperNounVP(properNoun.word, vp))
                         case (None, Some(rf: ReflexivePronoun), None) =>
                           Right(rf)
                         case (None, Some(rf: ReflexivePronoun), Some(vp)) =>
                           Right(ReflexivePronounVP(rf.word, vp))
                         case (Some(adj), Some(np), None) =>
                           Right(AdjectiveNounPhrase(adj, np))
                         case (Some(adj), Some(np), Some(vp)) =>
                           Right(AdjectiveNounPhraseVP(adj, np, vp))
                         case _ =>
                           Left(s"Invalid noun phrase:\n${tree.pennString()}")
                       }
                   }
        } yield result
    }

  def extractDeterminedNounPhrase(
    tree: Tree,
    verbOpt: Option[VerbPhrase]
  ): Either[String, NounPhrase] = {
    val existentialDeterminers = tree.children.flatMap { child =>
      child.label.value match {
        case "DT" if child.getChild(0).label.value.toLowerCase == "a" =>
          Some(ExistentialQuantifier.apply _)
        case "DT" if child.getChild(0).label.value.toLowerCase == "an" =>
          Some(ExistentialQuantifier.apply _)
        case "NNP" => Some(ExistentialQuantifier.apply _)
        case _     => None
      }
    }
    val forallDeterminers = tree.children.flatMap { child =>
      child.label.value match {
        case "DT" if child.getChild(0).label.value.toLowerCase == "every" =>
          Some(ForallQuantifier.apply _)
        case _ => None
      }
    }
    val existentialDeterminerOpt = existentialDeterminers.headOption
    val forallDeterminerOpt      = forallDeterminers.headOption
    (existentialDeterminerOpt, forallDeterminerOpt, verbOpt) match {
      case (None, None, _) =>
        extractAdjectiveNounPhrase(tree, verbOpt)
      case (Some(_), None, None) =>
        extractAdjectiveNounPhrase(tree, None).map(ExistentialQuantifier)
      case (Some(_), None, Some(vp)) =>
        extractAdjectiveNounPhrase(tree, None).map(ExistentialQuantifierVP(_, vp))
      case (None, Some(_), None) =>
        extractAdjectiveNounPhrase(tree, None).map(ForallQuantifier)
      case (None, Some(_), Some(vp)) =>
        extractAdjectiveNounPhrase(tree, None).map(ForallQuantifierVP(_, vp))
      case (Some(_), Some(_), _) =>
        Left("Forall determiner and existential determiner cannot be applied together")
    }
  }

  def extractNounPhrase(tree: Tree, verbOpt: Option[VerbPhrase]): Either[String, NounPhrase] =
    extractDeterminedNounPhrase(tree, verbOpt)

  def extractVerbPhrase(tree: Tree): Either[String, VerbPhrase] =
    tree.label.value match {
      case "VP" if tree.children.length == 1 =>
        Right(IntransitiveVerb(tree.getChild(0).getChild(0).label.value))
      case "VP" =>
        val verbWord = tree.getChild(0).getChild(0).label.value
        val leftChildren = tree.children.toList.filter(!_.label.value.startsWith("VB"))
        val nounPhrases = leftChildren.collect {
          case t if t.label.value == "NP" => extractNounPhrase(t, None)
        }
        nounPhrases match {
          case Nil =>
            leftChildren.head match {
              case t if t.label.value == "SBAR" => 
                for {
                  sentence <- convertSentence(t)
                } yield VerbSentencePhrase(verbWord, sentence)
              case t if t.label.value == "ADJP" =>
                for {
                  adjective <- extractAdjective(t)
                } yield VerbAdjectivePhrase(verbWord, adjective)
              case t if t.label.value == "ADVP" =>
                for {
                  adverb <- extractAdverb(t)
                } yield VerbAdverbPhrase(adverb.word, IntransitiveVerb(verbWord))
              case t if t.label.value == "PP" =>
                for {
                  propositionalPhrase <- extractPropositionalPhrase(t)
                } yield VerbInPhrase(propositionalPhrase, IntransitiveVerb(verbWord))
            }
          case Right(nounPhrase: NounPhraseWithoutVerbPhrase) :: Nil =>
            leftChildren.find(_.label.value != "NP") match {
              case Some(t) if t.label.value == "SBAR" => Left("Sentence cannot be applied to a transitive verb")
              case Some(t) if t.label.value == "ADJP" => Left("Adjective cannot be applied to a transitive verb")
              case Some(t) if t.label.value == "ADVP" =>
                for {
                  adverb <- extractAdverb(t)
                } yield VerbAdverbPhrase(adverb.word, TransitiveVerb(verbWord, nounPhrase))
              case Some(t) if t.label.value == "PP" =>
                for {
                  propositionalPhrase <- extractPropositionalPhrase(t)
                } yield VerbInPhrase(propositionalPhrase, TransitiveVerb(verbWord, nounPhrase))
              case None =>
                Right(TransitiveVerb(verbWord, nounPhrase))
            }
          case Left(error) :: _ =>
            Left(error)
          case _ =>
            Left("Unexpected number of noun phrases in verb phrase")
        }
    }

  def extractAdjective(tree: Tree): Either[String, Adjective] =
    tree.label.value match {
      case "ADJP" =>
        val adjectiveWords = tree.children.flatMap { child =>
          child.label.value match {
            case "JJ" =>
              Some(Adjective(child.getChild(0).label.value))
            case "RB" =>
              Some(Adjective(child.getChild(0).label.value)) // TODO: Adverbs are not adjectives
            case _ =>
              None
          }
        }
        val adjectiveWordOpt = adjectiveWords.headOption
        adjectiveWordOpt match {
          case Some(adjective) => Right(adjective)
          case None            => Left("Invalid adjective phrase")
        }
    }
  
  def extractAdverb(tree: Tree): Either[String, Adverb] = {
    tree.label.value match {
      case "ADVP" =>
        val adverbWords = tree.children.flatMap { child =>
          child.label.value match {
            case "RB" =>
              Some(Adverb(child.getChild(0).label.value))
            case _ =>
              None
          }
        }
        val adverbWordOpt = adverbWords.headOption
        adverbWordOpt match {
          case Some(adverb) => Right(adverb)
          case None         => Left("Invalid adverb phrase")
        }
    }
  }
  
  def extractPropositionalPhrase(tree: Tree): Either[String, PrepositionalPhrase] = {
    tree.label.value match {
      case "PP" =>
        if (tree.getChild(0).label.value == "IN") {
          val inWord = tree.getChild(0).getChild(0).label.value
          for {
            nounPhrase <- extractNounPhrase(tree.getChild(1), None)
          } yield InPhrase(inWord, nounPhrase)
        } else {
          Left("Illegal proposition")
        }
    }
  }

  def convertSentence(tree: Tree): Either[String, NounPhraseWithVerbPhrase] = {
    assert(tree.label.value == "ROOT" || tree.label.value == "SBAR")
    logger.debug(tree.pennString)
    val s = tree.getChild(0)
    assert(s.label.value == "S")
    val verbPhrase = s.children.find(_.label.value == "VP")
    val nounPhrase = s.children.find(_.label.value == "NP")
    val nounOpt = for {
      verb   <- verbPhrase.map(extractVerbPhrase).sequence[EitherS, VerbPhrase]
      result <- nounPhrase.map(extractNounPhrase(_, verb)).sequence[EitherS, NounPhrase]
    } yield result
    nounOpt match {
      case Right(Some(np: NounPhraseWithVerbPhrase)) =>
        Right(np)
      case Right(Some(_: NounPhraseWithoutVerbPhrase)) =>
        Left("Sentence should contain a verb phrase")
      case Right(Some(_)) =>
        Left("Invalid state")
      case Right(None) =>
        Left("Sentence should contain a noun phrase")
      case Left(error) =>
        Left(error)
    }
  }

  private def convert(tree: Tree): Either[String, NL] =
    tree.label.value match {
      case "SBAR" => convertSentence(tree)
      case "NP"   => extractNounPhrase(tree, None)
      case "ADJP" => extractAdjective(tree)
      case "ADVP" => extractAdverb(tree)
    }

  /**
   * Tries to parse a discourse into its representation in Sensala syntax trees.
   *
   * @param sentences list of discourse sentences parsed by the Stanford Parser
   * @return Right(discourse) if correctly parsed a syntax tree
    *        Left(error) if discourse is malformed
   */
  def parse(sentences: List[Tree]): Either[String, Discourse] = {
    for {
      result <- sentences
                 .map(convertSentence)
                 .sequence[EitherS, NounPhraseWithVerbPhrase]
    } yield Discourse(result)
  }
}
