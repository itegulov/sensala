package au.edu.anu.sensala.structure

trait L {
  def freeSymbols: Set[Sym]
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
