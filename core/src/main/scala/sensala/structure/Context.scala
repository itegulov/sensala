package sensala.structure

import org.aossie.scavenger.expression._

// TODO: make context more general, generic and abstract
case class Context(
  referents: List[Sym],
  boundSymbols: Set[Sym],
  conversions: List[(Sym, Sym)]
) {
  def findAnaphoricReferent: Option[Sym] = referents.headOption

  def addReferent(newRef: Sym): Context      = copy(referents = newRef :: referents)
  def deleteReferent(oldRef: Sym): Context   = copy(referents = referents.filterNot(_ == oldRef))
  def addBoundSym(sym: Sym): Context         = copy(boundSymbols = boundSymbols + sym)
  def addConversion(v: Sym, s: Sym): Context = copy(conversions = (v, s) :: conversions)
}
