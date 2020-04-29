package sensala

import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import cats.mtl.implicits._
import distage.Injector
import izumi.distage.model.definition.ModuleDef
import izumi.distage.model.plan.GCMode
import sensala.normalization.NormalFormConverter
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import sensala.error.NLError.FunctorRaiseNLError.raiseNLError
import sensala.shared.effect.Log
import sensala.interpreter.Interpreter
import sensala.parser.english._
import sensala.interpreter.context.{Context, LocalContext}
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.english.ParserError.HandleParserError.handleParserErrorIO
import sensala.property.{PropertyExtractor, WordNetPropertyExtractor}
import sensala.verbnet.VerbNetExtractor

object CLI extends IOApp {
  case class Config(discourse: String = "")

  private val parser = new scopt.OptionParser[Config]("sensala") {
    head(
      """
        |Sensala's Command Line Interface
        |
        |
      """.stripMargin
    )

    arg[String]("<discourse>...")
      .optional()
      .action { (v, c) =>
        c.copy(discourse = v)
      }
      .text("interpret <discourse>\n")

    help("help").text("print this usage text")

    note(
      """Example:
        |
        |sensala "John loves Mary"
      """.stripMargin
    )
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val c                     = parser.parse(args, Config()).get
    implicit val log: Log[IO] = Log.log[IO]
    for {
      implicit0(wordNetPropertyExtractor: WordNetPropertyExtractor[IO]) <- WordNetPropertyExtractor
                                                                            .create[IO]()
      implicit0(verbNetExtractor: VerbNetExtractor[IO])   <- VerbNetExtractor.create[IO]()
      implicit0(propertyExtractor: PropertyExtractor[IO]) = PropertyExtractor[IO]()
      implicit0(sensalaContext: Context[IO])              = Context.initial[IO]
      implicit0(sensalaLocalContext: LocalContext[IO])    = LocalContext.empty[IO]
      module = new ModuleDef {
        make[PronounParser[IO]]
        make[NounPhraseParser[IO]]
        make[VerbPhraseParser[IO]]
        make[DiscourseParser[IO]]
        addImplicit[Monad[IO]]
        addImplicit[HandleParserError[IO]]
      }
      plan     = Injector().plan(module, GCMode.NoGC)
      resource = Injector().produce(plan)
      implicit0(parser: DiscourseParser[IO]) = resource.use { objects =>
        objects.get[DiscourseParser[IO]]
      }
      interpreter = Interpreter[IO]()
      result <- HandleParserError[IO].attempt(DiscourseParser[IO].parse(c.discourse)).flatMap {
                 case Left(error) =>
                   Log[IO].error(
                     s"""Parsing failed:
                        |  $error
                     """.stripMargin
                   ) >> IO.pure(ExitCode.Error)
                 case Right(sentence) =>
                   for {
                     _ <- Log[IO].info(
                           s"""
                              |Result of sentence parsing:
                              |  $sentence
                           """.stripMargin
                         )
                     lambdaTerm <- interpreter.interpret(sentence, IO.pure(True))
                     context    <- sensalaContext.state.get
                     _ <- Log[IO].info(
                           s"""
                              |Result of discourse interpretation:
                              |  $lambdaTerm
                              |  ${lambdaTerm.pretty}
                           """.stripMargin
                         )
                     result = NormalFormConverter.normalForm(lambdaTerm)
                     _ <- Log[IO].info(
                           s"""
                              |Result of normalization:
                              |  $result
                              |  ${result.pretty}
                           """.stripMargin
                         )
                     prettyTerm = PrettyTransformer.transform(result)
                     _ <- Log[IO].info(
                           s"""
                              |Result of applying pretty
                              |  $prettyTerm
                              |  ${prettyTerm.pretty}
                           """.stripMargin
                         )
                     _ <- Log[IO].info(
                           s"""
                              |Context after interpretation:
                              |  ${context.entityProperties.map(_._2.pretty).mkString("\n")}
                           """.stripMargin
                         )
                     cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
                     _ <- Log[IO].info(
                           s"""
                              |Result of clausification:
                              |${cnf.clauses.mkString("\n")}
                           """.stripMargin
                         )
                   } yield ExitCode.Success
               }
    } yield result
  }
}
