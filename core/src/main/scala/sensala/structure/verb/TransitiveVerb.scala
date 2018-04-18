package sensala.structure.verb

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import org.atnos.eff.all.modify
import sensala.structure._
import sensala.structure.noun.NounPhrase
import sensala.structure.types._

final case class TransitiveVerb(
  word: String,
  obj: NounPhrase
) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- getEntity
      e <- bindFreeVar
      _ <- putEvent(e)
      w = Sym(word)
      objL <- obj.interpret(
               for {
                 y     <- getEntity
                 _     <- modify[NLFx, Context](_.addEvent(e, w(e) /\ agent(e, x) /\ patient(e, y)))
                 contL <- cont
               } yield Ex(e, event, w(e) /\ agent(e, x) /\ patient(e, y) /\ contL)
             )
    } yield objL
}
