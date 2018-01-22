package sensala

import cats.data.State
import org.atnos.eff._
import org.atnos.eff.all._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._

import scala.annotation.tailrec

package object structure {
  // State monad over the Context type aliases
  type StateContext[A] = State[Context, A]
  type EitherString[A] = Either[String, A]

  type CState = State[Context, E]

  type _stateContext[R] = StateContext |= R
  type _eitherString[R] = EitherString |= R

  type NLFx = Fx.fx2[StateContext, EitherString]
  type NLEff[A] = Eff[NLFx, A]
  type NLEffE = NLEff[E]

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
        val range  = ('a' to 'z').map(_.toString).map(Var.apply)
        bindFreeVarInternal(range)
      }
      _ <- put[NLFx, Context](context.addBoundSym(newSym))
    } yield newSym

  val nonHuman = Sym("non_human")
  val female   = Sym("female")
  val male     = Sym("male")
  val human    = Sym("human")

  implicit class ERich(val lambda: E) extends AnyVal {
    def pretty: String =
      lambda match {
        case Neg(b)           => s"¬${b.pretty}"
        case All(v, _, b)     => s"(∀${v.pretty}.${b.pretty})"
        case Ex(v, _, b)      => s"(∃${v.pretty}.${b.pretty})"
        case And(left, right) => s"${left.pretty} ∧ ${right.pretty}"
        case Or(left, right)  => s"${left.pretty} ∨ ${right.pretty}"
        case Imp(left, right) => s"${left.pretty} → ${right.pretty}"
        case Sym(name)        => name
        case AppRec(f, args)  => s"${f.pretty}(${args.map(_.pretty).mkString(", ")})"
        case App(f, a)        => s"(${f.pretty}(${a.pretty}))"
        case Abs(v, _, e)     => s"(λ${v.pretty}.${e.pretty})"
      }
  }
}
