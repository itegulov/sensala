package au.edu.anu.sensala

import cats.data.State

package object structure {
  // TODO: Perhaps we should eventually reuse Scavenger's data structures for logical expressions
  // TODO: extend this with articles, NL quantifiers, adverbs, ...

  // State monad over the Context type aliases
  type ContextState[A] = State[Context, A]
  type CState          = State[Context, L]

  val trueSym  = Sym("⊤")
  val falseSym = Sym("⊥")
  val negation = Sym("¬")
  val implies = Sym("→")
  val and      = Sym("∧")
  val forall   = Sym("∀")
  val exists   = Sym("∃")

  def bindFreeSym: ContextState[Sym] =
    State { context =>
      def getFreeSymInternal(range: Seq[Sym]): Sym =
        range.find(c => !context.boundSymbols.contains(c)) match {
          case Some(c) => c
          case _       => getFreeSymInternal(range.map(s => Sym(s.name + "'")))
        }
      val range  = ('a' to 'z').map(_.toString).map(Sym.apply)
      val newSym = getFreeSymInternal(range)
      (context.bindSym(newSym), newSym)
    }
}
