package sensala

import cats.data.State
import org.atnos.eff._
import org.atnos.eff.all._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.error.NLError

import scala.annotation.tailrec

package object structure {
  type StateContext[A]  = State[Context, A]
  type EitherNLError[A] = Either[NLError, A]

  type NLFx     = Fx.fx2[StateContext, EitherNLError]
  type NLEff[A] = Eff[NLFx, A]

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

    def apply(args: E*): E = AppRec(lambda, args)

    def \/:(right: E): E = Or(right, lambda)

    def /\:(right: E): E = And(right, lambda)
    
    def ->:(right: E): E = Imp(right, lambda)

    def unary_~(): E = Neg(lambda)
  }
}
