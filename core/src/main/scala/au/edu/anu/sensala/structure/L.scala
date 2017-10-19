package au.edu.anu.sensala.structure

trait L {
  def freeSymbols: Set[Sym]
  def pretty: String
}
case class Sym(name: String) extends L {
  override def freeSymbols: Set[Sym] = Set(this)
  override def pretty: String = name
}
case class App(f: L, a: L) extends L {
  // TODO: type checking
  override def freeSymbols: Set[Sym] = f.freeSymbols ++ a.freeSymbols

  def pretty: String = f.pretty + "(" + a.pretty + ")"
}

case class Abs(v: Sym, e: L) extends L {
  override def freeSymbols: Set[Sym] = e.freeSymbols - v
  override def pretty: String = s"\\${v.pretty}.${e.pretty}"
}
