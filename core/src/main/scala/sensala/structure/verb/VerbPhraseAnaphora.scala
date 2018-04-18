package sensala.structure.verb
import org.aossie.scavenger.expression.formula.{All, Ex}
import org.aossie.scavenger.expression.{E, Var}
import org.atnos.eff.all._
import sensala.error.{NLError, NLInvalidState}
import sensala.structure._
import sensala.structure.types.event

sealed trait Voice
case object Active extends Voice
case object Passive extends Voice

final case class VerbPhraseAnaphora(phrase: String, voice: Voice) extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      e <- gets[NLFx, Context, Var](_.findAnaphoricEvent(x, truth(x)).get)
      properties <- gets[NLFx, Context, E](_.eventProperties(e)) >>= {
                     case All(`e`, `event`, body) =>
                       pure(body)
                     case _ =>
                       left[NLFx, NLError, E](NLInvalidState("Unexpected properties format"))
                   }
      entity <- getEntity
      newE          <- bindFreeVar
      newProperties = voice match {
        case Active => substitute(substitute(properties, e, newE), agent, 1, entity)
        case Passive => substitute(substitute(properties, e, newE), patient, 1, entity)
      }
      _             <- putEvent(newE)
      _             <- modify[NLFx, Context](_.addEvent(newE, newProperties))
      contL         <- cont
    } yield Ex(newE, event, newProperties /\ contL)
}
