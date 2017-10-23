package au.edu.anu.sensala

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

package object structure {
  // TODO: Specify correct type for symbols
  // TODO: extend this with articles, NL quantifiers, adverbs, ...

  // State monad over the Context type aliases
  type ContextState[A] = State[Context, A]
  type CState          = State[Context, E]
  
  implicit class ERich(val lambda: E) extends AnyVal {
    def pretty: String =
      lambda match {
        case And(left, right) => s"${left.pretty} ∧ ${right.pretty}"
        case Sym(name) => name
        case App(App(App(f, a), b), c) => s"(${f.pretty}(${a.pretty}, ${b.pretty}, ${c.pretty}))"
        case App(App(f, a), b) => s"(${f.pretty}(${a.pretty}, ${b.pretty}))"
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
