package au.edu.anu.sensala.structure

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
  def interpret: CState =
    for {
      verbL <- verb.interpret
      objL  <- obj.interpret
      x     <- bindFreeSym
      y     <- bindFreeSym
    } yield Abs(x, App(objL, Abs(y, App(App(verbL, x), y))))
}