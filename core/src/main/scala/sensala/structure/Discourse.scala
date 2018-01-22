package sensala.structure

import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.True
import sensala.structure.noun.NounPhraseWithVerbPhrase
import org.atnos.eff._

final case class Discourse(sentences: List[NounPhraseWithVerbPhrase]) extends NL {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      result <- sentences.foldLeftM[NLEff, E](Abs(x, i, App(x, True))) {
        case (e, b) =>
          for {
            z      <- bindFreeVar
            a      <- bindFreeVar
            intRes <- b.interpret(Eff.pure(z))
          } yield Abs(z, i, App(e, Abs(a, i, intRes)))
      }
      contL <- cont
    } yield App(result, Abs(y, i, contL))
}
