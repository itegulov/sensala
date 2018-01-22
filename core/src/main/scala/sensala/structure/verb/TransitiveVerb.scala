package sensala.structure.verb

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.structure.noun.NounPhraseWithoutVerbPhrase

final case class TransitiveVerb(
  word: String,
  obj: NounPhraseWithoutVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
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
