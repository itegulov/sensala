package sensala.structure.verb

import org.aossie.scavenger.expression._
import sensala.structure.adjective.Adjective
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.structure.types._

final case class VerbAdjectivePhrase(
  verb: String,
  adjective: Adjective
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verb match {
      case "is" | "was" =>
        for {
          x <- bindFreeVar
          w = Sym(adjective.word)
          contL <- cont
        } yield Abs(x, entity, w(x) /\: contL(x))
      case other =>
        left[NLFx, NLError, E](NLUnexpectedWord(other))
    }
}
