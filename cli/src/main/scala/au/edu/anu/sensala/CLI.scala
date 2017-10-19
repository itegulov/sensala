package au.edu.anu.sensala

import au.edu.anu.sensala.normalization.NormalFormConverter
import au.edu.anu.sensala.parser.SentenceParser
import au.edu.anu.sensala.structure._
import com.typesafe.scalalogging.Logger

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
      val sentence = SentenceParser.parse(c.discourse)
      val (_, lambdaTerm) = sentence.interpret.run(Context(Nil, Set.empty)).value
      val result = NormalFormConverter.normalForm(lambdaTerm)
      logger.info(
        s"""
           |Result of discourse interpreting:
           |  ${lambdaTerm}
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
    }
  }
}
