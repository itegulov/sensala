package sensala

import cats.data.State
import org.atnos.eff._
import org.atnos.eff.all._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.error.NLError
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
    } yield localContext.entity.get

  def getEvent: NLEff[Var] =
    for {
      localContext <- get[NLFx, LocalContext]
    } yield localContext.event.get

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
          range.find(c => !context.boundSymbols.contains(c)) match {
            case Some(c) => c
            case _       => bindFreeVarInternal(range.map(s => Var(s.name + "'")))
          }
        val range = ('a' to 'z').map(_.toString).map(Var.apply)
        bindFreeVarInternal(range)
      }
      _ <- put[NLFx, Context](context.addBoundSym(newSym))
    } yield newSym

  val agent       = Sym("agent")
  val patient     = Sym("patient")
  val description = Sym("description")

  val named = Sym("named")

  val animal   = Sym("animal")
  val female   = Sym("female")
  val male     = Sym("male")
  val person   = Sym("person")

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

    def \/:(right: E): E = Or(right, lambda)

    def /\:(right: E): E = And(right, lambda)

    def ->:(right: E): E = Imp(right, lambda)

    def unary_~(): E = Neg(lambda)
  }
}
