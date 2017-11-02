package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import sensala.structure.{CState, Word, bindFreeVar}

final case class IntransitiveVerb(
  word: String
) extends Word with VerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, And(App(w, x), App(cont, x)))
}