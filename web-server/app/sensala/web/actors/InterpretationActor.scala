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
import sensala.parser.{DiscourseParser, SensalaStanfordParser}
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import sensala.structure.adjective.{Adjective, AdjectiveNounPhrase, AdjectiveNounPhraseVP}
import sensala.structure.noun._
import sensala.structure.prepositional.InPhrase
import sensala.structure.verb._
import sensala.structure.wh.WhNounPhrase
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
      case CommonNoun(word) =>
        SensalaNode(
          "CommonNoun",
          "type-commonnoun",
          List(atomNode(word))
        )
      case ProperNoun(word) =>
        SensalaNode(
          "ProperNoun",
          "type-propernoun",
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
      case ProperNounVP(word, vp) =>
        SensalaNode(
          "ProperNounVP",
          "type-propernoun",
          List(atomNode(word), convertNL(vp))
        )
      case ReflexivePronounVP(word, vp) =>
        SensalaNode(
          "ReflexivePronounVP",
          "type-reflpronoun",
          List(atomNode(word), convertNL(vp))
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
      case ForallQuantifierVP(np, vp) =>
        SensalaNode(
          "ForallQuantifierVP",
          "type-forall",
          List(convertNL(np), convertNL(vp))
        )
      case ExistentialQuantifierVP(np, vp) =>
        SensalaNode(
          "ExistentialQuantifierVP",
          "type-exists",
          List(convertNL(np), convertNL(vp))
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
          List(atomNode(adverb), convertNL(verbPhrase))
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
      case VerbPhraseAnaphora(phrase) =>
        SensalaNode(
          "VerbPharseAnaphora",
          "type-verbphraseanaphora",
          List(atomNode(phrase))
        )
      case WhNounPhrase(verbPhrase, nounPhrase) =>
        SensalaNode(
          "WhNounPhrase",
          "type-whnounphrase",
          List(convertNL(verbPhrase), convertNL(nounPhrase))
        )
      case InPhrase(word, nounPhrase) =>
        SensalaNode(
          "InPhrase",
          "type-inphrase",
          List(atomNode(word), convertNL(nounPhrase))
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
      case AdjectiveNounPhraseVP(adjective, nounPhrase, verbPhrase) =>
        SensalaNode(
          "AdjectiveNounPhraseVP",
          "type-adjectivenounphrase",
          List(convertNL(adjective), convertNL(nounPhrase), convertNL(verbPhrase))
        )
    }
  }

  def connected(outgoing: ActorRef): Receive = {
    case IncomingMessage(message) =>
      message.validate[SensalaInterpretMessage] match {
        case JsSuccess(SensalaRunInterpretation(discourse), _) =>
          val sentences = SensalaStanfordParser.parse(discourse)
          log.info(
            s"""
               |Result of Stanford parsing:
               |  $sentences
                """.stripMargin
          )
          outgoing ! OutgoingMessage(Json.toJson(StanfordParsed(convertTree(sentences.head))))
          val parsed = Try(DiscourseParser.parse(sentences))
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
                  .runState[Context](Context(Map.empty, Map.empty, Set.empty))
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
                       |  ${context.referentProperties.map(_._2.pretty).mkString("\n")}
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
