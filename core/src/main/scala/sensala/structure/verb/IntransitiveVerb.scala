package sensala.structure.verb

import org.aossie.scavenger.expression._
import sensala.structure._

final case class IntransitiveVerb(
  word: String
) extends Word with VerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      w = Sym(word)
      contL <- cont
    } yield Abs(x, i, w(x) /\ contL(x))
}
