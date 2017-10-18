package au.edu.anu.sensala

import au.edu.anu.sensala.parser.SentenceParser
import au.edu.anu.sensala.structure._

object CLI {
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
      println(SentenceParser.parse(c.discourse).interpret(new Context(Nil)))
    }
  }
}
