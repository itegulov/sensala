package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.formula.{All, Ex}
import org.aossie.scavenger.expression.E
import sensala.error.NLInvalidState
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types.event

sealed trait Voice
case object Active extends Voice
case object Passive extends Voice

final case class VerbPhraseAnaphora[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  phrase: String,
  voice: Voice
) extends VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      e <- Context[F].findAnaphoricEventUnsafe(List.empty)
      properties <- Context[F].eventProperties(e) >>= {
                     case All(`e`, `event`, body) =>
                       Monad[F].pure[E](body)
                     case _ =>
                       FunctorRaiseNLError[F].raise[E](
                         NLInvalidState("Unexpected properties format")
                       )
                   }
      entity <- LocalContext[F].getEntity
      newE          <- Context[F].bindFreeVar
      newProperties = voice match {
        case Active => substitute(substitute(properties, e, newE), agent, 1, entity)
        case Passive => substitute(substitute(properties, e, newE), patient, 1, entity)
      }
      _             <- LocalContext[F].putEvent(newE)
      _             <- Context[F].addEvent(newE, newProperties)
      contL         <- cont
    } yield Ex(newE, event, newProperties /\ contL)
}
