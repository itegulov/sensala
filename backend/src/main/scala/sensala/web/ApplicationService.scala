package sensala.web

import cats.{Functor, Monad}
import cats.implicits._
import cats.effect._
import cats.mtl.FunctorRaise
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
import sensala.interpreter.Interpreter
import org.http4s.twirl._
import sensala.shared.effect.Log
import sensala.error.NLError
import sensala.error.NLError.FunctorRaiseNLError
import sensala.interpreter.context.{Context, LocalContext}
import sensala.models.SensalaNode
import sensala.normalization.NormalFormConverter
import sensala.parser.english.EnglishDiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.property.{PropertyExtractor, WordNetPropertyExtractor}
import sensala.structure._
import sensala.models._

import scala.util.Try

final case class ApplicationService[F[_]: Sync: Concurrent: Interpreter: Log]()
    extends Http4sDsl[F] {
  object DiscourseQueryParamMatcher extends QueryParamDecoderMatcher[String]("discourse")

  private def convertTree(tree: Tree): SensalaNode =
    SensalaNode(
      tree.label.value,
      if (tree.children.toList.isEmpty) "type-TK" else "type-" + tree.label.value,
      tree.children.toList.map(convertTree)
    )

  def evalDiscourseSimple(discourse: String): F[List[SensalaInterpretMessage]] = {
    val sentences = EnglishDiscourseParser.buildPennTaggedTree(discourse)
    for {
      _              <- Log[F].info(s"Result of Stanford parsing:\n$sentences")
      stanfordParsed = StanfordParsed(convertTree(sentences.head))
      parsed = Try(EnglishDiscourseParser.parse(discourse))
        .getOrElse(Left("Invalid sentence"))
      result <- parsed match {
                 case Left(error) =>
                   for {
                     _ <- Log[F].error(s"Parsing failed:\n$error")
                   } yield List(stanfordParsed, SensalaError(s"Parsing failed: $error"))
                 case Right(sentence) =>
                   implicit val raiseNLError: FunctorRaiseNLError[F] =
                     new FunctorRaise[F, NLError] {
                       override val functor: Functor[F] = Functor[F]

                       override def raise[A](e: NLError): F[A] =
                         throw new RuntimeException(e.toString)
                     }
                   WordNetPropertyExtractor.create[F]().flatMap {
                     implicit wordNetPropertyExtractor =>
                       implicit val propertyExtractor: PropertyExtractor[F] =
                         PropertyExtractor[F]()
                       implicit val sensalaContext: Context[F] = Context.initial[F]
                       implicit val sensalaLocalContext: LocalContext[F] =
                         LocalContext.empty[F]
                       for {
                         _             <- Log[F].info(s"Result of sentence parsing:\n$sentence")
                         sensalaParsed = SensalaParsed(sentence)
                         interpreter   = Interpreter[F]()
                         lambdaTerm    <- interpreter.interpret(sentence, Monad[F].pure(True))
                         context       <- sensalaContext.state.get
                         _ <- Log[F].info(
                               s"""
                                  |Result of discourse interpretation:
                                  |  $lambdaTerm
                                  |  ${lambdaTerm.pretty}
                                 """.stripMargin
                             )
                         normalForm = NormalFormConverter.normalForm(lambdaTerm)
                         _ <- Log[F].info(
                               s"""
                                  |Result of applying β-reduction:
                                  |  $normalForm
                                  |  ${normalForm.pretty}
                                 """.stripMargin
                             )
                         prettyTerm = PrettyTransformer.transform(normalForm)
                         _ <- Log[F].info(
                               s"""
                                  |Result of applying pretty transform:
                                  |  ${prettyTerm.pretty}
                                 """.stripMargin
                             )
                         entitiyProperties = context.entityProperties.map(_._2.pretty)
                         _ <- Log[F].info(
                               s"""
                                  |Context after interpretation:
                                  |  ${entitiyProperties.mkString("\n")}
                                 """.stripMargin
                             )
                         cnf = new TPTPClausifier()
                           .apply(List((prettyTerm, AxiomClause)))
                         _ <- Log[F].info(
                               s"""
                                  |Result of clausification:
                                  |${cnf.clauses.mkString("\n")}
                                 """.stripMargin
                             )
                       } yield
                         List(stanfordParsed, sensalaParsed, SensalaInterpreted(prettyTerm.pretty))
                   }
               }
    } yield result
  }

  val application = HttpRoutes.of[F] {
    case GET -> Root =>
      Ok(sensala.html.index())
    case POST -> Root / "eval" :? DiscourseQueryParamMatcher(discourse) =>
      for {
        message <- evalDiscourseSimple(discourse)
        result  <- Ok(message.asJson.noSpaces)
      } yield result
  }
}
