package sensala.structure

import org.aossie.scavenger.expression._

case class WhNounPhrase(verbPhrase: VerbPhrase, nounPhrase: NounPhrase) extends NounPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeSym
      x <- bindFreeSym
      q <- nounPhrase.interpret
      r <- verbPhrase.interpret
      p <- bindFreeSym
    } yield Abs(x, i, Abs(f, i, App(App(q, x), App(App(r, Abs(p, i, App(p, x))), f))))
}
