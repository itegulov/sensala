package au.edu.anu.sensala

import cats.data.State
import org.aossie.scavenger.expression._

package object structure {
  // TODO: Specify correct type for symbols
  // TODO: extend this with articles, NL quantifiers, adverbs, ...

  // State monad over the Context type aliases
  type ContextState[A] = State[Context, A]
  type CState          = State[Context, E]

  val trueSym  = Sym("⊤")
  val falseSym = Sym("⊥")
  val negation = Sym("¬")
  val implies = Sym("→")
  val and      = Sym("∧")
  val forall   = Sym("∀")
  val exists   = Sym("∃")

  object And {
    def apply(left: E, right: E): E = App(App(Sym("∧"), left), right)

    def unapply(arg: E): Option[(E, E)] = arg match {
      case App(App(Sym("∧"), left), right) => Some((left, right))
      case _ => None
    }
  }
  
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
