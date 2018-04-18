package sensala.web.actors

import akka.actor.{Actor, ActorLogging, ActorRef}
import edu.stanford.nlp.trees.Tree
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import org.atnos.eff.Eff
import org.atnos.eff.syntax.all._
import play.api.libs.json._
import sensala.error.NLError
import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.adverb._
import sensala.structure.noun._
import sensala.structure.prepositional._
import sensala.structure.verb._
import sensala.structure.wh._
import sensala.web.actors.InterpretationActor.{Connected, IncomingMessage, OutgoingMessage}
import sensala.web.shared._

import scala.util.Try

case class InterpretationActor() extends Actor with ActorLogging {
  override def receive: Receive = {
    case Connected(actorRef) =>
      context.become(connected(actorRef))
  }

  private def convertTree(tree: Tree): SensalaNode =
    SensalaNode(
      tree.label.value,
      if (tree.children.toList.isEmpty) "type-TK" else "type-" + tree.label.value,
      tree.children.toList.map(convertTree)
    )

  private def atomNode(word: String): SensalaNode =
    SensalaNode(
      word,
      "type-word",
      Nil
    )

  private def convertNL(nl: NL): SensalaNode = {
    nl match {
      case Discourse(sentences) =>
        SensalaNode(
          "Discourse",
          "type-discourse",
          sentences.map(convertNL)
        )
      case Sentence(np, vp) =>
        SensalaNode(
          "Sentence",
          "type-sentence",
          List(convertNL(np), convertNL(vp))
        )
      case CommonNoun(word) =>
        SensalaNode(
          "CommonNoun",
          "type-commonnoun",
          List(atomNode(word))
        )
      case ProperNoun(word, Some(typ), Some(gender)) =>
        SensalaNode(
          "ProperNoun",
          "type-propernoun",
          List(atomNode(s"$word $typ $gender"))
        )
      case ProperNoun(word, Some(typ), None) =>
        SensalaNode(
          "ProperNoun",
          "type-propernoun",
          List(atomNode(s"$word $typ"))
        )
      case ProperNoun(word, None, Some(gender)) =>
        SensalaNode(
          "ProperNoun",
          "type-propernoun",
          List(atomNode(s"$word $gender"))
        )
      case ProperNoun(word, None, None) =>
        SensalaNode(
          "ProperNoun",
          "type-propernoun",
          List(atomNode(s"$word"))
        )
      case PossessivePronoun(word) =>
        SensalaNode(
          "PossessivePronoun",
          "type-posspronoun",
          List(atomNode(word))
        )
      case ReflexivePronoun(word) =>
        SensalaNode(
          "ReflexivePronoun",
          "type-reflpronoun",
          List(atomNode(word))
        )
      case DemonstrativePronoun(word) =>
        SensalaNode(
          "DemonstrativePronoun",
          "type-demopronoun",
          List(atomNode(word))
        )
      case ForallQuantifier(np) =>
        SensalaNode(
          "ForallQuantifier",
          "type-forall",
          List(convertNL(np))
        )
      case ExistentialQuantifier(np) =>
        SensalaNode(
          "ExistentialQuantifier",
          "type-exists",
          List(convertNL(np))
        )
      case DefiniteNounPhrase(np) =>
        SensalaNode(
          "DefiniteNounPhrase",
          "type-definitenounphrase",
          List(convertNL(np))
        )
      case IntransitiveVerb(word) =>
        SensalaNode(
          "IntransitiveVerb",
          "type-intransitive",
          List(atomNode(word))
        )
      case TransitiveVerb(word, np) =>
        SensalaNode(
          "TransitiveVerb",
          "type-transitive",
          List(atomNode(word), convertNL(np))
        )
      case VerbAdjectivePhrase(verb, adjective) =>
        SensalaNode(
          "VerbAdjectivePhrase",
          "type-verbadjphrase",
          List(atomNode(verb), convertNL(adjective))
        )
      case VerbAdverbPhrase(adverb, verbPhrase) =>
        SensalaNode(
          "VerbAdverbPhrase",
          "type-verbadverbphrase",
          List(atomNode(adverb.word), convertNL(verbPhrase))
        )
      case VerbInPhrase(propositionalPhrase, verbPhrase) =>
        SensalaNode(
          "VerbInPhrase",
          "type-verbinphrase",
          List(convertNL(propositionalPhrase), convertNL(verbPhrase))
        )
      case VerbSentencePhrase(word, sentence) =>
        SensalaNode(
          "VerbSentencePhrase",
          "type-verbsentencephrase",
          List(atomNode(word), convertNL(sentence))
        )
      case VerbPhraseAnaphora(phrase, voice) =>
        SensalaNode(
          "VerbPharseAnaphora",
          "type-verbphraseanaphora",
          List(atomNode(phrase), atomNode(voice.toString))
        )
      case WhNounPhrase(verbPhrase, nounPhrase) =>
        SensalaNode(
          "WhNounPhrase",
          "type-whnounphrase",
          List(convertNL(verbPhrase), convertNL(nounPhrase))
        )
      case NounPhrasePreposition(prepositionalPhrase, nounPhrase) =>
        SensalaNode(
          "NounPhrasePreposition",
          "type-nounphrasepreposition",
          List(convertNL(prepositionalPhrase), convertNL(nounPhrase))
        )
      case InPhrase(word, nounPhrase) =>
        SensalaNode(
          "InPhrase",
          "type-inphrase",
          List(atomNode(word), convertNL(nounPhrase))
        )
      case PossessionPhrase(nounPhrase) =>
        SensalaNode(
          "PossessionPhrase",
          "type-possessionphrase",
          List(convertNL(nounPhrase))
        )
      case Adjective(word) =>
        SensalaNode(
          "Adjective",
          "type-adjective",
          List(atomNode(word))
        )
      case AdjectiveNounPhrase(adjective, nounPhrase) =>
        SensalaNode(
          "AdjectiveNounPhrase",
          "type-adjectivenounphrase",
          List(convertNL(adjective), convertNL(nounPhrase))
        )
    }
  }

  def connected(outgoing: ActorRef): Receive = {
    case IncomingMessage(message) =>
      message.validate[SensalaInterpretMessage] match {
        case JsSuccess(SensalaRunInterpretation(discourse), _) =>
          val sentences = DiscourseParser.buildPennTaggedTree(discourse)
          log.info(
            s"""
               |Result of Stanford parsing:
               |  $sentences
            """.stripMargin
          )
          outgoing ! OutgoingMessage(Json.toJson(StanfordParsed(convertTree(sentences.head))))
          val parsed = Try(DiscourseParser.parse(discourse))
            .getOrElse(Left("Invalid sentence (maybe a grammatical mistake?)"))
          parsed match {
            case Left(error) =>
              log.error(
                s"""Parsing failed:
                   |  $error
                """.stripMargin
              )
              outgoing ! OutgoingMessage(Json.toJson(SensalaError(s"Parsing failed: $error")))
            case Right(sentence) =>
              log.info(
                s"""
                   |Result of sentence parsing:
                   |  $sentence
                """.stripMargin
              )
              outgoing ! OutgoingMessage(Json.toJson(SensalaParsed(convertNL(sentence))))
              val ((lambdaTermEither, context), localContext) =
                sentence
                  .interpret(Eff.pure(True))
                  .runEither[NLError]
                  .runState[Context](Context.initial)
                  .runState[LocalContext](LocalContext.empty)
                  .run
              lambdaTermEither match {
                case Left(error) =>
                  log.error(
                    s"""Interpreting failed:
                       |  $error
                    """.stripMargin
                  )
                  outgoing ! OutgoingMessage(
                    Json.toJson(SensalaError(s"Interpreting failed: $error"))
                  )
                case Right(lambdaTerm) =>
                  log.info(
                    s"""
                       |Result of discourse interpretation:
                       |  $lambdaTerm
                       |  ${lambdaTerm.pretty}
                    """.stripMargin
                  )
                  val result = NormalFormConverter.normalForm(lambdaTerm)
                  log.info(
                    s"""
                       |Result of applying β-reduction:
                       |  $result
                       |  ${result.pretty}
                    """.stripMargin
                  )
                  val prettyTerm = PrettyTransformer.transform(result)
                  log.info(
                    s"""
                       |Result of applying pretty transform:
                       |  ${prettyTerm.pretty}
                    """.stripMargin
                  )
                  log.info(
                    s"""
                       |Context after interpretation:
                       |  ${context.entityProperties.map(_._2.pretty).mkString("\n")}
                    """.stripMargin
                  )
                  val cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
                  log.info(
                    s"""
                       |Result of clausification:
                       |${cnf.clauses.mkString("\n")}
                    """.stripMargin
                  )
                  outgoing ! OutgoingMessage(Json.toJson(SensalaInterpreted(prettyTerm.pretty)))
              }
          }
        case JsSuccess(other, _) =>
          log.warning(s"Unexpected message from ${sender()}: $other")
          outgoing ! OutgoingMessage(
            Json.toJson(SensalaError(s"Unexpected message from ${sender()}: $other!"))
          )
        case JsError(error) =>
          log.warning(s"Error while parsing json: $error")
          outgoing ! OutgoingMessage(Json.toJson(SensalaError(s"Error while parsing json: $error")))
      }
  }
}

object InterpretationActor {
  final case class Connected(actorRef: ActorRef)
  final case class IncomingMessage(message: JsValue)
  final case class OutgoingMessage(message: JsValue)
}
