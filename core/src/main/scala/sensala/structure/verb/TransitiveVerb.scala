package sensala.structure.verb

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import sensala.structure.{CState, bindFreeVar}
import sensala.structure.noun.NounPhraseWithoutVerbPhrase

final case class TransitiveVerb(
  word: String,
  obj: NounPhraseWithoutVerbPhrase
) extends VerbPhrase {
  def interpret(cont: CState): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(word)
      objL  <- obj.interpret(
        for {
          contL <- cont
        } yield Abs(y, i, And(AppRec(w, List(x, y)), App(contL, x)))
      )
    } yield Abs(x, i, objL)
}
