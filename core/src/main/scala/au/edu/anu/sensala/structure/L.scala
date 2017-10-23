package au.edu.anu.sensala.structure

trait L {
  def freeSymbols: Set[Sym]
  def pretty: String =
    this match {
      case And(left, right) => s"${left.pretty} ∧ ${right.pretty}"
      case Sym(name) => name
      case App(App(App(f, a), b), c) => s"(${f.pretty}(${a.pretty}, ${b.pretty}, ${c.pretty}))"
      case App(App(f, a), b) => s"(${f.pretty}(${a.pretty}, ${b.pretty}))"
      case App(f, a) => s"(${f.pretty}(${a.pretty}))"
      case Abs(v, e) => s"(λ${v.pretty}.${e.pretty})"
    }
}

case class Sym(name: String) extends L {
  override def freeSymbols: Set[Sym] = Set(this)
}

case class App(f: L, a: L) extends L {
  // TODO: type checking
  override def freeSymbols: Set[Sym] = f.freeSymbols ++ a.freeSymbols
}

case class Abs(v: Sym, e: L) extends L {
  override def freeSymbols: Set[Sym] = e.freeSymbols - v
}

object And {
  def apply(left: L, right: L): L = App(App(Sym("∧"), left), right)

  def unapply(arg: L): Option[(L, L)] = arg match {
    case App(App(Sym("∧"), left), right) => Some((left, right))
    case _ => None
  }
}
