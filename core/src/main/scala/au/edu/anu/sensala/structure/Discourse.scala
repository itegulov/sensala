package au.edu.anu.sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression.{App, E}
import org.aossie.scavenger.expression.formula.True

case class Discourse(phrases: List[NL]) extends NL {
  override def interpret: CState =
    for {
      sentenceLs <- phrases.map(_.interpret).sequence[ContextState, E]
    } yield sentenceLs.foldRight[E](True)(App.apply)
}
