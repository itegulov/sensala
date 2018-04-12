package sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression._
import org.atnos.eff._
import sensala.structure.types._

final case class Discourse(sentences: List[Sentence]) extends NL {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      result <- sentences.foldLeftM[NLEff, E](Abs(x, i, x)) {
                 case (e, b) =>
                   for {
                     z      <- bindFreeVar
                     _      <- flushLocalContext()
                     intRes <- b.interpret(Eff.pure(z))
                   } yield Abs(z, entity, e(intRes))
               }
      contL <- cont
    } yield result(contL)
}
