package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure._
import sensala.structure.types._

final case class IntransitiveVerb(
  word: String
) extends Word
    with VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- getEntity
      e <- bindFreeVar
      _ <- putEvent(e)
      w = Sym(word)
      contL <- cont
    } yield Ex(e, event, w(e) /\: agent(e, x) /\: contL)
}
