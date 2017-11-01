package sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.True

case class Discourse(sentences: List[NounPhraseWithVerbPhrase]) extends NL {
  override def interpret: CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      sentenceLs <- sentences.map(_.interpret).sequence[ContextState, E]
    } yield sentenceLs.foldRight[E](True) { case (a, b) => App(a, Abs(y, i, b)) }
}
