package sensala.structure

case class Sentence(subject: NounPhrase, verbPhrase: VerbPhrase) extends NL {
  def interpret(c: Context) = {
    val s = subject.interpret(c)
    val nc = new Context(s::c.referents)
    App(verbPhrase.interpret(nc), s)
  }
}
