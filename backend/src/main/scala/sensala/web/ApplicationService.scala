package sensala.web

import cats.{Functor, Monad}
import cats.implicits._
import cats.effect._
import cats.mtl.FunctorRaise
import com.typesafe.scalalogging.Logger
import edu.stanford.nlp.trees.Tree
import io.circe.syntax._
import io.circe.parser._
import fs2._
import fs2.concurrent.Queue
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.server.websocket.WebSocketBuilder
import sensala.interpreter.Interpreter
import org.http4s.twirl._
import org.http4s.websocket.WebSocketFrame
import org.http4s.websocket.WebSocketFrame.Text
import sensala.effect.Capture
import sensala.error.NLError
import sensala.error.NLError.FunctorRaiseNLError
import sensala.interpreter.context.{Context, LocalContext}
import sensala.models.SensalaNode
import sensala.normalization.NormalFormConverter
import sensala.parser.english.EnglishDiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.property.PropertyExtractor
import sensala.structure._
import sensala.models._

import scala.util.Try

final case class ApplicationService[F[_]: Sync: Concurrent: Capture: Interpreter]()
    extends Http4sDsl[F] {
  private val log = Logger[this.type]
  object DiscourseQueryParamMatcher extends QueryParamDecoderMatcher[String]("discourse")

  private def convertTree(tree: Tree): SensalaNode =
    SensalaNode(
      tree.label.value,
      if (tree.children.toList.isEmpty) "type-TK" else "type-" + tree.label.value,
      tree.children.toList.map(convertTree)
    )

  val application = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok(sensala.html.index())
    case GET -> Root / "ws" =>
      val sensalaReply: Pipe[F, WebSocketFrame, WebSocketFrame] =
        _.flatMap {
          case Text(discourseString, _) =>
            parse(discourseString) match {
              case Right(discourseJson) =>
                discourseJson.as[SensalaInterpretMessage] match {
                  case Right(SensalaRunInterpretation(discourse)) =>
                    val sentences = EnglishDiscourseParser.buildPennTaggedTree(discourse)
                    log.info(
                      s"""
                         |Result of Stanford parsing:
                         |  $sentences
                  """.stripMargin
                    )
                    val result1 =
                      Text(
                        (StanfordParsed(convertTree(sentences.head)): SensalaInterpretMessage).asJson.toString
                      )
                    val parsed = Try(EnglishDiscourseParser.parse(discourse))
                      .getOrElse(Left("Invalid sentence (maybe a grammatical mistake?)"))
                    parsed match {
                      case Left(error) =>
                        log.error(
                          s"""Parsing failed:
                             |  $error
                      """.stripMargin
                        )
                        val result2 =
                          Text(
                            (SensalaError(s"Parsing failed: $error"): SensalaInterpretMessage).asJson.toString
                          )
                        Stream[F, WebSocketFrame](
                          result1,
                          result2
                        )
                      case Right(sentence) =>
                        log.info(
                          s"""
                             |Result of sentence parsing:
                             |  $sentence
                      """.stripMargin
                        )
                        val result2 =
                          Text(
                            (SensalaParsed(sentence): SensalaInterpretMessage).asJson.toString
                          )

                        implicit val raiseNLError: FunctorRaiseNLError[F] =
                          new FunctorRaise[F, NLError] {
                            override val functor: Functor[F] = Functor[F]

                            override def raise[A](e: NLError): F[A] =
                              throw new RuntimeException(e.toString)
                          }
                        implicit val propertyExtractor: PropertyExtractor[F] =
                          PropertyExtractor[F]()
                        implicit val sensalaContext: Context[F]           = Context.initial[F]
                        implicit val sensalaLocalContext: LocalContext[F] = LocalContext.empty[F]
                        val interpreter                                   = Interpreter[F]()
                        Stream[F, WebSocketFrame](result1, result2) ++ Stream.eval(
                          for {
                            lambdaTerm   <- interpreter.interpret(sentence, Monad[F].pure(True))
                            context      <- sensalaContext.state.get
                            localContext <- sensalaLocalContext.state.get
                            _ = log.info(
                              s"""
                                 |Result of discourse interpretation:
                                 |  $lambdaTerm
                                 |  ${lambdaTerm.pretty}
                          """.stripMargin
                            )
                            normalForm = NormalFormConverter.normalForm(lambdaTerm)
                            _ = log.info(
                              s"""
                                 |Result of applying β-reduction:
                                 |  $normalForm
                                 |  ${normalForm.pretty}
                          """.stripMargin
                            )
                            prettyTerm = PrettyTransformer.transform(normalForm)
                            _ = log.info(
                              s"""
                                 |Result of applying pretty transform:
                                 |  ${prettyTerm.pretty}
                          """.stripMargin
                            )
                            _ = log.info(
                              s"""
                                 |Context after interpretation:
                                 |  ${context.entityProperties.map(_._2.pretty).mkString("\n")}
                          """.stripMargin
                            )
                            cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
                            _ = log.info(
                              s"""
                                 |Result of clausification:
                                 |${cnf.clauses.mkString("\n")}
                          """.stripMargin
                            )
                            result = (SensalaInterpreted(prettyTerm.pretty): SensalaInterpretMessage).asJson
                          } yield Text(result.toString): WebSocketFrame
                        )
                    }
                  case Right(other) =>
                    log.error(s"Unexpected message: $other")
                    Stream[F, WebSocketFrame](
                      Text(
                        (SensalaError(s"Unexpected message: $other!"): SensalaInterpretMessage).asJson.toString
                      )
                    )
                  case Left(error) =>
                    log.error(s"Invalid SensalaInterpretMessage JSON: $error")
                    Stream[F, WebSocketFrame](
                      Text(
                        (SensalaError(s"Invalid SensalaInterpretMessage JSON: $error"): SensalaInterpretMessage).asJson.toString
                      )
                    )
                }
              case Left(error) =>
                log.error(s"Invalid JSON: $error")
                Stream[F, WebSocketFrame](
                  Text(
                    (SensalaError(s"Invalid JSON: $error"): SensalaInterpretMessage).asJson.toString
                  )
                )
            }
        }

      Queue
        .unbounded[F, WebSocketFrame]
        .flatMap { q =>
          for {
            x <- q.dequeue
          } yield x
          val d = q.dequeue.through(sensalaReply)
          val e = q.enqueue
          WebSocketBuilder[F].build(d, e)
        }
  }
}
