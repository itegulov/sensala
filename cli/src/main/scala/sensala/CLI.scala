package sensala

import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.structure._
import com.typesafe.scalalogging.Logger
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause

import scala.util.control.Breaks

object CLI {
  private val logger = Logger[this.type]

  case class Config(discourse: String = "")

  private val parser = new scopt.OptionParser[Config]("sensala") {
    head("\nSensala's Command Line Interface\n\n")

    arg[String]("<discourse>...") optional () action { (v, c) =>
      c.copy(discourse = v)
    } text "interpret <discourse>\n"

    help("help") text "print this usage text"

    note("""
    Example:

      sensala "John loves Mary"
      """)
  }

  
  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()) foreach { c =>
      val sentence = DiscourseParser.parse(c.discourse)
      val (context1, lambdaTerm) = sentence.interpret.run(Context(Nil, Set.empty, Nil)).value
      val (context2, normalizedTerm) = NormalFormConverter.normalForm(lambdaTerm).run(context1).value
      val (context, result) = applyConversions(normalizedTerm).run(context2).value
      val prettyTerm = PrettyTransformer.transform(result)
      val cnf = new TPTPClausifier().apply(List((prettyTerm, AxiomClause)))
      logger.info(
        s"""
           |Context after interpretation:
           |  ${context.referents}
        """.stripMargin
      )
      logger.info(
        s"""
           |Result of sentence parsing:
           |  $sentence
        """.stripMargin
      )
      logger.info(
        s"""
           |Result of discourse interpretation:
           |  $lambdaTerm
           |  ${lambdaTerm.pretty}
        """.stripMargin
      )
      logger.info(
        s"""
           |Result of applying β-reduction:
           |  $result
           |  ${result.pretty}
        """.stripMargin
      )
      logger.info(
        s"""
           |Result of applying pretty transform:
           |  ${prettyTerm.pretty}
        """.stripMargin
      )
      logger.info(
        s"""
           |Result of clausification:
           |${cnf.clauses.mkString("\n")}
        """.stripMargin
      )
    }
  }
}
