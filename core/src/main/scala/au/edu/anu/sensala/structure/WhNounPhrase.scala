package au.edu.anu.sensala.structure

case class WhNounPhrase(verbPhrase: VerbPhrase, nounPhrase: NounPhrase) extends NounPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeSym
      x <- bindFreeSym
      q <- nounPhrase.interpret
      r <- verbPhrase.interpret
      p <- bindFreeSym
    } yield Abs(x, Abs(f, App(App(q, x), App(App(r, Abs(p, App(p, x))), f))))
}
