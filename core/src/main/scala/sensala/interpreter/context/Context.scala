package sensala.interpreter.context

import cats.Monad
import cats.mtl.MonadState
import cats.implicits._
import monix.execution.atomic.AtomicAny
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{All, And}
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.prover.{EPCR, Unsatisfiable}
import org.aossie.scavenger.structure.immutable.{AxiomClause, NegConjectureClause}
import sensala.shared.effect.{AtomicMonadState, Capture}
import sensala.error.NLError.FunctorRaiseNLError
import sensala.error.NLUnknownAnaphoricReferent
import sensala.property.Property
import sensala.types.{entity, event}
import sensala.structure._

import scala.annotation.tailrec
import scala.concurrent.duration._

private[context] final case class ContextState(
  entityProperties: Map[Var, E],
  eventProperties: Map[Var, E],
  boundVars: Set[Var]
)

final case class Context[F[_]: Monad: FunctorRaiseNLError] private[context] (
  state: MonadState[F, ContextState]
) {
  private val clausifier = new TPTPClausifier()

  private def convertPropertiesToE(x: Var, properties: List[Property]): E = {
    val propertiesE = properties.map(p => p.propertyExp(x))
    if (propertiesE.isEmpty) truth(x) else propertiesE.reduceLeft(_ /\ _)
  }

  private def convertSymsToE(x: Var, properties: List[Sym]): E = {
    val propertiesE = properties.map(_.apply(x))
    if (propertiesE.isEmpty) truth(x) else propertiesE.reduceLeft(_ /\ _)
  }

  def findAnaphoricEntity(v: Var, properties: E): F[Option[Var]] =
    state.get.map(_.entityProperties.find {
      case (_, refProperties) =>
        val cnf = clausifier(
          List(
            (refProperties, AxiomClause),
            (All(v, entity, ~properties), NegConjectureClause)
          )
        )
        EPCR.prove(cnf, 1 seconds) match {
          case Unsatisfiable(_) => true
          case _                => false
        }
    }.map(_._1))

  def findAnaphoricEntityUnsafe(v: Var, properties: E): F[Var] =
    findAnaphoricEntity(v, properties).flatMap {
      case Some(ref) => ref.pure[F]
      case None      => FunctorRaiseNLError[F].raise(NLUnknownAnaphoricReferent(properties))
    }

  def findAnaphoricEntity(properties: List[Property]): F[Option[Var]] =
    for {
      x           <- bindFreeVar
      propertiesE = convertPropertiesToE(x, properties)
      result      <- findAnaphoricEntity(x, propertiesE)
    } yield result

  def findAnaphoricEntityUnsafe(properties: List[Property]): F[Var] =
    findAnaphoricEntity(properties).flatMap {
      case Some(ref) => ref.pure[F]
      case None =>
        for {
          x           <- bindFreeVar
          propertiesE = convertPropertiesToE(x, properties)
          result      <- FunctorRaiseNLError[F].raise[Var](NLUnknownAnaphoricReferent(propertiesE))
        } yield result
    }

  def findAnaphoricEvent(v: Var, properties: E): F[Option[Var]] =
    state.get.map(_.eventProperties.find {
      case (_, evProperties) =>
        val cnf = clausifier(
          List(
            (evProperties, AxiomClause),
            (All(v, event, ~properties), NegConjectureClause)
          )
        )
        EPCR.prove(cnf, 5 seconds) match {
          case Unsatisfiable(_) => true
          case _                => false
        }
    }.map(_._1))

  def findAnaphoricEvent(properties: List[Sym]): F[Option[Var]] =
    for {
      x           <- bindFreeVar
      propertiesE = convertSymsToE(x, properties)
      result      <- findAnaphoricEvent(x, propertiesE)
    } yield result

  def findAnaphoricEventUnsafe(properties: List[Sym]): F[Var] =
    findAnaphoricEvent(properties).flatMap {
      case Some(ref) => ref.pure[F]
      case None =>
        for {
          x           <- bindFreeVar
          propertiesE = convertSymsToE(x, properties)
          result      <- FunctorRaiseNLError[F].raise[Var](NLUnknownAnaphoricReferent(propertiesE))
        } yield result
    }

  def eventProperties(event: Var): F[E] =
    state.get.map(_.eventProperties(event))

  def addEntity(newRef: Var, properties: List[Property]): F[Unit] =
    state.modify { context =>
      context.copy(
        entityProperties = context.entityProperties.updated(
          newRef,
          All(newRef, entity, properties.map(p => p.propertyExp(newRef)).reduceLeft(And.apply))
        )
      )
    }

  def addEvent(newEvent: Var, properties: List[Property]): F[Unit] =
    state.modify { context =>
      context.copy(
        eventProperties = context.eventProperties.updated(
          newEvent,
          All(newEvent, event, properties.map(p => p.propertyExp(newEvent)).reduceLeft(And.apply))
        )
      )
    }

  def addEvent(newEvent: Var, properties: E): F[Unit] =
    state.modify { context =>
      context.copy(
        eventProperties = context.eventProperties.updated(
          newEvent,
          All(newEvent, event, properties)
        )
      )
    }

  def deleteReferent(oldRef: Var): F[Unit] =
    state.modify { context =>
      context.copy(
        entityProperties = context.entityProperties - oldRef
      )
    }

  def addBoundVar(v: Var): F[Unit] =
    state.modify { context =>
      context.copy(
        boundVars = context.boundVars + v
      )
    }

  def bindFreeVar: F[Var] =
    for {
      context <- state.get
      newSym = {
        @tailrec
        def bindFreeVarInternal(range: Seq[Var]): Var =
          range.find(c => !context.boundVars.contains(c)) match {
            case Some(c) => c
            case _       => bindFreeVarInternal(range.map(s => Var(s.name + "'")))
          }
        val range = ('a' to 'z').map(_.toString).map(Var.apply)
        bindFreeVarInternal(range)
      }
      _ <- addBoundVar(newSym)
    } yield newSym
}

object Context {
  def apply[F[_]](implicit ev: Context[F]): Context[F] = ev

  def initial[F[_]: Monad: Capture: FunctorRaiseNLError]: Context[F] = {
    val speakerEntity      = Var("speaker")
    val interlocutorEntity = Var("interlocutor")
    val state = ContextState(
      Map(
        speakerEntity      -> speaker(speakerEntity),
        interlocutorEntity -> interlocutor(interlocutorEntity),
      ),
      Map.empty,
      Set(speakerEntity, interlocutorEntity)
    )
    Context[F](new AtomicMonadState(AtomicAny(state)))
  }
}
