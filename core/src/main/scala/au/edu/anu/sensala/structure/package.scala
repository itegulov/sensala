package au.edu.anu.sensala

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._

package object structure {
  // TODO: Specify correct type for symbols
  // TODO: extend this with articles, NL quantifiers, adverbs, ...

  // State monad over the Context type aliases
  type ContextState[A] = State[Context, A]
  type CState          = State[Context, E]
  
  implicit class ERich(val lambda: E) extends AnyVal {
    def pretty: String =
      lambda match {
        case Neg(b) => s"¬${b.pretty}"
        case All(v, _, b) => s"(∀${v.pretty}.${b.pretty})"
        case Ex(v, _, b) => s"(∃${v.pretty}.${b.pretty})"
        case And(left, right) => s"${left.pretty} ∧ ${right.pretty}"
        case Or(left, right) => s"${left.pretty} ∨ ${right.pretty}"
        case Imp(left, right) => s"${left.pretty} → ${right.pretty}"
        case Sym(name) => name
        case AppRec(f, args) => s"(${f.pretty}(${args.map(_.pretty).mkString(", ")})"
        case App(f, a) => s"(${f.pretty}(${a.pretty}))"
        case Abs(v, _, e) => s"(λ${v.pretty}.${e.pretty})"
      }
  }

  def bindFreeSym: ContextState[Var] =
    State { context =>
      def getFreeSymInternal(range: Seq[Var]): Var =
        range.find(c => !context.boundSymbols.contains(c)) match {
          case Some(c) => c
          case _       => getFreeSymInternal(range.map(s => Var(s.name + "'")))
        }
      val range  = ('a' to 'z').map(_.toString).map(Var.apply)
      val newSym = getFreeSymInternal(range)
      (context.bindSym(newSym), newSym)
    }
}
