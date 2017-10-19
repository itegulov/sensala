package au.edu.anu.sensala.structure

// TODO: make context more general, generic and abstract
case class Context(referents: List[Sym], boundSymbols: Set[Sym]) {
  def findAnaphoricReferent: Option[Sym] = referents.headOption
  def extend(newRef: Sym): Context = Context(newRef :: referents, boundSymbols)
  def bindSym(sym: Sym): Context = Context(referents, boundSymbols + sym)
}
