package au.edu.anu.sensala

import au.edu.anu.sensala.normalization.NormalFormConverter
import au.edu.anu.sensala.parser.DiscourseParser
import au.edu.anu.sensala.postprocessing.PrettyTransformer
import au.edu.anu.sensala.structure._
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
      var result = normalizedTerm
      var context = context2
      // TODO: find a monadic way to do this
      Breaks.breakable {
        while (true) {
          val (newContext, newResult) = context.applyConversions(result).run(context).value
          if (result == newResult) {
            Breaks.break
          }
          result = newResult
          context = newContext
        }
      }
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
