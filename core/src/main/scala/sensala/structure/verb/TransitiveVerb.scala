package sensala.structure.verb

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import sensala.structure.{CState, bindFreeVar}
import sensala.structure.noun.NounPhraseWithoutVerbPhrase

final case class TransitiveVerb(
  word: String,
  obj: NounPhraseWithoutVerbPhrase
) extends VerbPhrase {
  def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(word)
      objL  <- obj.interpret(Abs(y, i, And(AppRec(w, List(x, y)), App(cont, x))))
    } yield Abs(x, i, objL)
}
