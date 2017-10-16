package sensala.structure

trait VerbPhrase extends NL {
  def apply(subject: NounPhrase) = Sentence(subject, this)

  def \:(subject: NounPhrase) = apply(subject)
}

case class TransitiveVerb(word: String) extends Word {
  def apply(obj: NounPhrase) = VerbObjPhrase(this, obj)

  def /(obj: NounPhrase) = apply(obj)
}

case class IntransitiveVerb(word: String) extends Word with VerbPhrase

case class VerbObjPhrase(verb: TransitiveVerb, obj: NounPhrase) extends VerbPhrase {
  def interpret(e: Context) = App(verb.interpret(e), obj.interpret(e))
}