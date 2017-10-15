package object sensala {

  // TODO: Organize all this nicely in separate packages


  // TODO: make context more general, generic and abstract
  class Context(val referents: List[Sym]) {
    def findAnaphoricReferent() = referents.head // FIXME : This throws exception when list is empty
  }

  // Logical Expressions // TODO: Perhaps we should eventually reuse Scavenger's data structures for logical expressions
  trait T
  case object i extends T
  case object o extends T
  case class ->(a: T, b: T) extends T

  trait L
  case class Sym(s: String) extends L
  case class App(f: L, a: L) extends L  // TODO: type checking
  case class Abs(v: Sym, e: L) extends L

  // Natural Language Expressions

  trait NL {
    def interpret(e: Context): L
  }

  trait Word extends NL {
    val word: String
    def interpret(e: Context) = Sym(word)
  }

  // TODO: extend this with articles, NL quantifiers, adverbs, ...


  trait NounPhrase extends NL {
    def interpret(e: Context): Sym
  }

  case class Noun(word: String) extends Word with NounPhrase

  trait VerbPhrase extends NL {
    def apply(subject: Noun) = Sentence(subject, this)

    def \:(subject: Noun) = apply(subject)
  }

  case class Sentence(subject: NounPhrase, verbPhrase: VerbPhrase) extends NL {
    def interpret(c: Context) = {
      val s = subject.interpret(c)
      val nc = new Context(s::c.referents)
      App(verbPhrase.interpret(nc), s)
    }
  }

  case class TransitiveVerb(word: String) extends Word {
    def apply(obj: Noun) = VerbObjPhrase(this, obj)

    def /(obj: Noun) = apply(obj)
  }

  case class IntransitiveVerb(word: String) extends Word with VerbPhrase

  case class VerbObjPhrase(verb: TransitiveVerb, obj: NounPhrase) extends VerbPhrase {
    def interpret(e: Context) = App(verb.interpret(e), obj.interpret(e))
  }

  case class ReflexivePronoun(word: String) extends Word with NounPhrase {
    override def interpret(e: Context) = e.findAnaphoricReferent()
  }


  def parse(s: String): Sentence = ??? // TODO


  // Some Examples...

  val john = Noun("John")

  val mary = Noun("Mary")

  val runs = IntransitiveVerb("runs")

  val loves = TransitiveVerb("loves")

  val s = (loves(mary))(john)

  val s2 = john \: (loves / mary)

  val s3 = john \: runs

  val input = "John loves Mary"

  //val parsed = parse(input)
  val parsed = Sentence(Noun("John"), VerbObjPhrase(TransitiveVerb("loves"), Noun("Mary")))

  // TODO: write proper test classes for these assertions
  assert(parsed.interpret(new Context(Nil)) == App(App(Sym("loves"), Sym("Mary")), Sym("John")))
}
