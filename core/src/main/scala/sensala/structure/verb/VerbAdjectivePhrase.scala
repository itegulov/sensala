package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure.adjective.Adjective
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.Property
import sensala.structure.types.event

final case class VerbAdjectivePhrase(
  verb: String,
  adjective: Adjective
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verb match {
      case "is" | "was" =>
        for {
          x     <- getEntity
          e     <- bindFreeVar
          _     <- putEvent(e)
          w     = Sym(adjective.word)
          _     <- modify[NLFx, Context](_.addEvent(e, List(Property(w))))
          contL <- cont
        } yield Ex(e, event, description(e) /\: w(e, x) /\: contL)
      case other =>
        left[NLFx, NLError, E](NLUnexpectedWord(other))
    }
}
