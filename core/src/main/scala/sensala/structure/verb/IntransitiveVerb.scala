package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{Ex, True}
import sensala.structure._
import sensala.structure.types._
import org.atnos.eff.all._
import sensala.property.Property

final case class IntransitiveVerb(
  word: String
) extends Word
    with VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      e     <- bindFreeVar
      _     <- putEvent(e)
      w     = Sym(word)
      _     <- modify[NLFx, Context](_.addEvent(e, List(Property(w))))
      contL <- cont
    } yield Ex(e, event, w(e) /\: agent(e, x) /\: contL)
}
