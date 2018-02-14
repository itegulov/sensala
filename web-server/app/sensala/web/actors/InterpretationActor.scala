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
import sensala.property.{CachedPropertyExtractor, ConceptNetPropertyExtractor}
import sensala.structure._
import sensala.web.actors.InterpretationActor.{Connected, IncomingMessage, OutgoingMessage}
import sensala.web.shared._

case class InterpretationActor() extends Actor with ActorLogging {

  implicit val propertyExtractor = CachedPropertyExtractor(ConceptNetPropertyExtractor)
  val discourseParser            = DiscourseParser()

  override def receive: Receive = {
    case Connected(actorRef) =>
      context.become(connected(actorRef))
  }
  
  private def convertTree(tree: Tree): StanfordNode = {
    StanfordNode(
      tree.label.value,
      if (tree.children.toList.isEmpty) "type-TK" else "type-" + tree.label.value,
      tree.children.toList.map(convertTree)
    )
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
          val parsed = discourseParser.parse(sentences)
          parsed match {
            case Left(error) =>
              log.error(
                s"""Parsing failed:
                   |  $error
                """.stripMargin
              )
              Json.toJson(SensalaError("Parsing failed!"))
            case Right(sentence) =>
              log.info(
                s"""
                   |Result of sentence parsing:
                   |  $sentence
                """.stripMargin
              )
              outgoing ! OutgoingMessage(Json.toJson(SensalaParsed(sentence.toString)))
              val ((lambdaTermEither, context), localContext) =
                sentence
                  .interpret(Eff.pure(True))
                  .runEither[NLError]
                  .runState[Context](Context(Map.empty, Set.empty))
                  .runState[LocalContext](LocalContext.empty)
                  .run
              val lambdaTerm = lambdaTermEither match {
                case Right(e) => e
                case Left(error) => sys.error(s"Erorr: $error")
              }
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
        case JsSuccess(other, _) =>
          log.warning(s"Unexpected message from ${sender()}: $other")
          outgoing ! OutgoingMessage(Json.toJson(SensalaError(s"Unexpected message from ${sender()}: $other!")))
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
