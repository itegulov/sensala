package sensala

import cats.Functor
import cats.mtl.FunctorRaise
import sensala.normalization.NormalFormConverter
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import com.typesafe.scalalogging.Logger
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import sensala.error.NLError
import sensala.interpreter.Interpreter
import sensala.parser.english.EnglishDiscourseParser
import sensala.interpreter.context.{Context, LocalContext}

object CLI {
  private val logger = Logger[this.type]

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

  def main(args: Array[String]): Unit =
    parser.parse(args, Config()).foreach { c =>
      implicit val raiseNLError = new FunctorRaise[Task, NLError] {
        override val functor: Functor[Task] = Functor[Task]

        override def raise[A](e: NLError): Task[A] =
          throw new RuntimeException(e.toString)
      }
      implicit val sensalaContext      = Context.initial[Task]
      implicit val sensalaLocalContext = LocalContext.empty[Task]
      val interpreter                  = Interpreter[Task]()
      EnglishDiscourseParser.parse(c.discourse) match {
        case Left(error) =>
          logger.error(
            s"""Parsing failed:
               |  $error
            """.stripMargin
          )
        case Right(sentence) =>
          logger.info(
            s"""
               |Result of sentence parsing:
               |  $sentence
            """.stripMargin
          )
          val (lambdaTerm, context, localContext) =
            (for {
              lambdaTerm   <- interpreter.interpret(sentence, Task.pure(True))
              context      <- sensalaContext.state.get
              localContext <- sensalaLocalContext.state.get
            } yield (lambdaTerm, context, localContext)).runSyncUnsafe()
          logger.info(
            s"""
               |Result of discourse interpretation:
               |  $lambdaTerm
               |  ${lambdaTerm.pretty}
            """.stripMargin
          )
          val result = NormalFormConverter.normalForm(lambdaTerm)
          logger.info(
            s"""
               |Result of applying β-reduction:
               |  $result
               |  ${result.pretty}
            """.stripMargin
          )
          val prettyTerm = PrettyTransformer.transform(result)
          logger.info(
            s"""
               |Result of applying pretty transform:
               |  ${prettyTerm.pretty}
            """.stripMargin
          )
          logger.info(
            s"""
               |Context after interpretation:
               |  ${context.entityProperties.map(_._2.pretty).mkString("\n")}
            """.stripMargin
          )
          val cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
          logger.info(
            s"""
               |Result of clausification:
               |${cnf.clauses.mkString("\n")}
            """.stripMargin
          )
      }
    }
}
