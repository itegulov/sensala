package sensala.structure

trait L
case class Sym(s: String) extends L
case class App(f: L, a: L) extends L  // TODO: type checking
case class Abs(v: Sym, e: L) extends L
