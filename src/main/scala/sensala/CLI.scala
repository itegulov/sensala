package sensala

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
      val s1 = Sentence(Noun("John"), VerbObjPhrase(TransitiveVerb("loves"), Noun("Mary")))
      assert(s1.interpret(new Context(Nil)) == App(App(Sym("loves"), Sym("Mary")), Sym("John")))
      println(s1.interpret(new Context(Nil)))

      val s2 = Sentence(Noun("Mary"), VerbObjPhrase(TransitiveVerb("loves"), ReflexivePronoun("herself")))
      assert(s2.interpret(new Context(Nil)) == App(App(Sym("loves"), Sym("Mary")), Sym("Mary")))
      println(s2.interpret(new Context(Nil)))


    }
  }
}
