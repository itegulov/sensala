package sensala.structure.verb

import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.structure.noun.NounPhraseWithoutVerbPhrase
import sensala.structure.types._

final case class TransitiveVerb(
  word: String,
  obj: NounPhraseWithoutVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(word)
      objL <- obj.interpret(
               for {
                 contL <- cont
               } yield Abs(y, entity, w(x, y) /\: contL(x))
             )
    } yield Abs(x, entity, objL)
}
