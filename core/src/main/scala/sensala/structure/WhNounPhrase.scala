package sensala.structure

import org.aossie.scavenger.expression._

case class WhNounPhrase(verbPhrase: VerbPhrase, nounPhrase: NounPhrase) extends NounPhrase {
  override def interpret: CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      q <- nounPhrase.interpret
      r <- verbPhrase.interpret
      p <- bindFreeVar
    } yield Abs(x, i, Abs(f, i, App(App(q, x), App(App(r, Abs(p, i, App(p, x))), f))))

  override def gender = nounPhrase.gender
}
