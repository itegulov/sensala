package sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.True
import sensala.structure.noun.NounPhraseWithVerbPhrase

final case class Discourse(sentences: List[NounPhraseWithVerbPhrase]) extends NL {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      result <- sentences.foldLeftM[ContextState, E](Abs(x, i, App(x, True))) {
                 case (e, b) =>
                   for {
                     z      <- bindFreeVar
                     a      <- bindFreeVar
                     intRes <- b.interpret(z)
                   } yield Abs(z, i, App(e, Abs(a, i, intRes)))
               }
    } yield App(result, Abs(y, i, cont))
}
