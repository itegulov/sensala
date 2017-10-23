package sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression.{App, E}
import org.aossie.scavenger.expression.formula.True

case class Discourse(sentences: List[Sentence]) extends NL {
  override def interpret: CState =
    for {
      sentenceLs <- sentences.map(_.interpret).sequence[ContextState, E]
    } yield sentenceLs.foldRight[E](True)(App.apply)
}
