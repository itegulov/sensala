package sensala

import cats.data.State
import org.atnos.eff._
import org.atnos.eff.all._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.error.{NLError, NLInvalidState, NLUnknownAnaphoricReferent}
import sensala.property.Property
import sensala.structure.types._

import scala.annotation.tailrec

package object structure {
  type StateContext[A]      = State[Context, A]
  type StateLocalContext[A] = State[LocalContext, A]
  type EitherNLError[A]     = Either[NLError, A]

  type NLFx     = Fx.fx3[StateContext, StateLocalContext, EitherNLError]
  type NLEff[A] = Eff[NLFx, A]

  def getEntity: NLEff[Var] =
    for {
      localContext <- get[NLFx, LocalContext]
      entity <- localContext.entity match {
        case Some(e) => right[NLFx, NLError, Var](e)
        case None => left[NLFx, NLError, Var](NLInvalidState("Local entity could not be found"))
      }
    } yield entity

  def getEvent: NLEff[Var] =
    for {
      localContext <- get[NLFx, LocalContext]
      event <- localContext.event match {
        case Some(e) => right[NLFx, NLError, Var](e)
        case None => left[NLFx, NLError, Var](NLInvalidState("Local event could not be found"))
      }
    } yield event

  def putEntity(v: Var): NLEff[Unit] =
    for {
      localContext <- get[NLFx, LocalContext]
      _            <- put[NLFx, LocalContext](localContext.copy(entity = Some(v)))
    } yield ()

  def putEvent(v: Var): NLEff[Unit] =
    for {
      localContext <- get[NLFx, LocalContext]
      _            <- put[NLFx, LocalContext](localContext.copy(event = Some(v)))
    } yield ()

  def flushLocalContext(): NLEff[Unit] = put[NLFx, LocalContext](LocalContext(None, None))

  def bindFreeVar: NLEff[Var] =
    for {
      context <- get[NLFx, Context]
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
      _ <- put[NLFx, Context](context.addBoundVar(newSym))
    } yield newSym
  
  def findAnaphoricEntity(properties: E, x: Var): NLEff[Var] =
    for {
      refOpt <- gets[NLFx, Context, Option[Var]](_.findAnaphoricEntity(x, properties))
      ref <- refOpt match {
        case Some(ref) => right[NLFx, NLError, Var](ref)
        case None => left[NLFx, NLError, Var](NLUnknownAnaphoricReferent(properties))
      }
    } yield ref

  def findAnaphoricEntityOpt(properties: E, x: Var): NLEff[Option[Var]] =
    gets[NLFx, Context, Option[Var]](_.findAnaphoricEntity(x, properties))

  def findAnaphoricEntity(properties: List[Property]): NLEff[Var] =
    for {
      x <- bindFreeVar
      propertiesE = properties.map(p => p.propertyExp(x)).foldLeft(truth(x))(_ /\ _)
      result <- findAnaphoricEntity(propertiesE, x)
    } yield result

  def findAnaphoricEntityOpt(properties: List[Property]): NLEff[Option[Var]] =
    for {
      x <- bindFreeVar
      propertiesE = properties.map(p => p.propertyExp(x)).foldLeft(truth(x))(_ /\ _)
      result <- findAnaphoricEntityOpt(propertiesE, x)
    } yield result
  
  def findAnaphoricEvent(properties: List[Sym]): NLEff[Var] =
    for {
      x <- bindFreeVar
      propertiesE = properties.map(_.apply(x)).foldRight(truth(x))(_ /\ _)
      refOpt <- gets[NLFx, Context, Option[Var]](_.findAnaphoricEvent(x, propertiesE))
      ref <- refOpt match {
        case Some(ref) => right[NLFx, NLError, Var](ref)
        case None => left[NLFx, NLError, Var](NLUnknownAnaphoricReferent(propertiesE))
      }
    } yield ref

  val agent       = Sym("agent")
  val patient     = Sym("patient")
  val description = Sym("description")

  val named = Sym("named")

  val animal = Sym("animal")
  val location = Sym("location")
  val person = Sym("person")
  val organization = Sym("organization")
  val money = Sym("money")
  val percent = Sym("percent")
  val date = Sym("date")
  val time = Sym("time")
  
  val male   = Sym("male")
  val female = Sym("female")
  
  // Special Sensala properties
  val speaker = Sym("sensala_speaker")
  val interlocutor = Sym("sensala_interlocutor")
  
  // FIXME: Make True a case object in Scavenger
  val Truth: E = True

  // FIXME: find a better way to represent truth and false
  def truth(x: Sym): E = person(x) \/ ~person(x)

  def substitute(exp: E, old: Sym, newE: E): E = exp match {
    case `old`                       => newE
    case s: Sym                      => s
    case Abs(v, t, body) if v != old => Abs(v, t, substitute(body, old, newE))
    case abs: Abs                    => abs
    case App(f, a)                   => App(substitute(f, old, newE), substitute(a, old, newE))
  }

  def substitute(exp: E, predicate: Sym, position: Int, newE: E): E = exp match {
    case AppRec(`predicate`, args) if position < args.size =>
      AppRec(predicate, args.toList.patch(position, List(newE), 1))
    case s: Sym =>
      s
    case Abs(v, t, body) =>
      Abs(v, t, substitute(body, predicate, position, newE))
    case App(f, a) =>
      App(substitute(f, predicate, position, newE), substitute(a, predicate, position, newE))
  }

  implicit class ERich(val lambda: E) extends AnyVal {
    def pretty: String =
      lambda match {
        case Neg(b)                      => s"¬${b.pretty}"
        case All(v, t, b) if t == entity => s"(∀${v.pretty}ᵉⁿ.${b.pretty})"
        case All(v, t, b) if t == event  => s"(∀${v.pretty}ᵉᵛ.${b.pretty})"
        case Ex(v, t, b) if t == entity  => s"(∃${v.pretty}ᵉⁿ.${b.pretty})"
        case Ex(v, t, b) if t == event   => s"(∃${v.pretty}ᵉᵛ.${b.pretty})"
        case And(left, right)            => s"${left.pretty} ∧ ${right.pretty}"
        case Or(left, right)             => s"${left.pretty} ∨ ${right.pretty}"
        case Imp(left, right)            => s"${left.pretty} → ${right.pretty}"
        case Sym(name)                   => name
        case AppRec(f, args)             => s"${f.pretty}(${args.map(_.pretty).mkString(", ")})"
        case App(f, a)                   => s"(${f.pretty}(${a.pretty}))"
        case Abs(v, _, e)                => s"(λ${v.pretty}.${e.pretty})"
      }

    def apply(args: E*): E = AppRec(lambda, args)

    def \/(right: E): E = Or(lambda, right)

    def /\(right: E): E = And(lambda, right)

    def ->:(right: E): E = Imp(right, lambda)

    def unary_~(): E = Neg(lambda)
  }
}
