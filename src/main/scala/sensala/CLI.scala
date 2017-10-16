package sensala

import sensala.parser.SentenceParser
import sensala.structure._

object CLI {

  var hjafhjaf = "kafvkjba"

  case class Config(discourse: String = "")

  val parser = new scopt.OptionParser[Config]("sensala") {
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

      // TODO: write proper test classes for these assertions
      val s1 = SentenceParser.parse("John walks")
      assert(s1.interpret(new Context(Nil)) == App(Sym("walks"), Sym("John")))
      println(s1.interpret(new Context(Nil)))

      val s2 = SentenceParser.parse("John walks dog")
      assert(s2.interpret(new Context(Nil)) == App(App(Sym("walks"), Sym("John")), Sym("dog")))
      println(s2.interpret(new Context(Nil)))

      val s3 = SentenceParser.parse("Mary loves herself")
      assert(s3.interpret(new Context(Nil)) == App(App(Sym("loves"), Sym("Mary")), Sym("Mary")))
      println(s3.interpret(new Context(Nil)))
    }
  }
}
