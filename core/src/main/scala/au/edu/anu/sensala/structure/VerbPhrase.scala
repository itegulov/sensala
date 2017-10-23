package au.edu.anu.sensala.structure

trait VerbPhrase extends NL

case class TransitiveVerb(word: String) extends Word {
  override def interpret: CState =
    for {
      f <- bindFreeSym
      x <- bindFreeSym
      y <- bindFreeSym
      p <- bindFreeSym
      q <- bindFreeSym
    } yield Abs(p, Abs(q, App(q, Abs(x, App(p, Abs(y, Abs(f, And(App(App(Sym(word), x), y), f))))))))
}

case class IntransitiveVerb(word: String) extends Word with VerbPhrase {
  override def interpret =
    for {
      f <- bindFreeSym
      p <- bindFreeSym
      q <- bindFreeSym
      x <- bindFreeSym
      y <- bindFreeSym
    } yield Abs(p, Abs(q, App(q, Abs(x, App(p, Abs(y, Abs(f, And(App(App(Sym(word), x), y), f))))))))
}

case class VerbObjPhrase(verb: TransitiveVerb, obj: NounPhrase) extends VerbPhrase {
  def interpret: CState =
    for {
      verbL <- verb.interpret
      objL  <- obj.interpret
    } yield App(verbL, objL)
}
