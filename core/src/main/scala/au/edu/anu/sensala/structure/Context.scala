package au.edu.anu.sensala.structure

import au.edu.anu.sensala.normalization.NormalFormConverter
import cats.data.State

// TODO: make context more general, generic and abstract
case class Context(referents: List[Sym], boundSymbols: Set[Sym], conversions: List[(Sym, Sym)]) {
  def applyConversions(lambda: L): CState = conversions.foldLeft[CState](State.pure(lambda)) { 
    case (lambdaL, (v, s)) => lambdaL.flatMap(NormalFormConverter.substitute(_, v, s))
  }
  def findAnaphoricReferent: Option[Sym] = referents.headOption
  def extend(newRef: Sym): Context = Context(newRef :: referents, boundSymbols,conversions)
  def deleteReferent(oldRef: Sym): Context = Context(referents.filterNot(_ == oldRef), boundSymbols, conversions)
  def bindSym(sym: Sym): Context = Context(referents, boundSymbols + sym, conversions)
  def addConversion(v: Sym, s: Sym): Context = Context(referents, boundSymbols, (v, s) :: conversions)
}
