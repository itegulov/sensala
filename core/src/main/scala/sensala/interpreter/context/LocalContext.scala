package sensala.interpreter.context

import cats.Monad
import cats.mtl.MonadState
import cats.implicits._
import monix.execution.atomic.AtomicAny
import org.aossie.scavenger.expression.Var
import sensala.effect.{AtomicMonadState, Capture}
import sensala.error.NLError.FunctorRaiseNLError
import sensala.error.NLInvalidState

private[context] final case class LocalContextState(
  entity: Option[Var],
  event: Option[Var]
)

final case class LocalContext[F[_]: Monad: FunctorRaiseNLError] private[context] (
  state: MonadState[F, LocalContextState]
) {
  def getEntity: F[Var] =
    for {
      localContext <- state.get
      entity <- localContext.entity match {
                 case Some(e) => e.pure[F]
                 case None =>
                   FunctorRaiseNLError[F].raise(NLInvalidState("Local entity could not be found"))
               }
    } yield entity

  def getEvent: F[Var] =
    for {
      localContext <- state.get
      event <- localContext.event match {
                case Some(e) => e.pure[F]
                case None =>
                  FunctorRaiseNLError[F].raise(NLInvalidState("Local event could not be found"))
              }
    } yield event

  def putEntity(v: Var): F[Unit] =
    state.modify(_.copy(entity = Some(v)))

  def putEvent(v: Var): F[Unit] =
    state.modify(_.copy(event = Some(v)))

  def clear: F[Unit] =
    state.set(LocalContextState(None, None))
}

object LocalContext {
  def apply[F[_]](implicit ev: LocalContext[F]): LocalContext[F] = ev

  def empty[F[_]: Monad: Capture: FunctorRaiseNLError]: LocalContext[F] =
    LocalContext(
      new AtomicMonadState(
        AtomicAny(LocalContextState(None, None))
      )
    )
}
