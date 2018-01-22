package sensala

import cats.data.State
import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import com.typesafe.scalalogging.Logger
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._
import sensala.property.PropertyExtractor
import sensala.structure.noun.{ExistentialQuantifier, ExistentialQuantifierVP, ProperNoun}
import sensala.structure.verb.TransitiveVerb

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

    arg[String]("<discourse>...") optional () action { (v, c) =>
      c.copy(discourse = v)
    } text "interpret <discourse>\n"

    help("help") text "print this usage text"

    note(
      """Example:
        |
        |sensala "John loves Mary"
      """.stripMargin
    )
  }

  
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      val parsed = DiscourseParser.parse(c.discourse)
      parsed match {
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
          val (lambdaTermEither, context) = sentence.interpret(Eff.pure(True)).runEither[String].runState[Context](Context(Map.empty, Set.empty)).run
          val lambdaTerm = lambdaTermEither.right.get
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
               |  ${context.referentProperties.map(_._2.pretty).mkString("\n")}
            """.stripMargin
          )
          val cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
          logger.info(
            s"""
               |Result of clausification:
               |${cnf.clauses.mkString("\n")}
            """.stripMargin
          )
          PropertyExtractor.close()
      }
    }
  }
}
