package sensala.structure

import org.aossie.scavenger.expression._

// TODO: make context more general, generic and abstract
case class Context(
  referentProperties: Map[Sym, E],
  boundSymbols: Set[Sym]
) {
  def findAnaphoricReferent(properties: E): Option[Sym] =
    referentProperties.find {
      case (_, refProperties) =>
        properties =+= refProperties
    }.map(_._1)
  def addReferent(newRef: Sym, properties: E): Context =
    copy(referentProperties = referentProperties.updated(newRef, properties))
  def addReferent(newRef: Var, gender: Gender): Context =
    gender match {
      case Male   => 
        addReferent(newRef, Abs(newRef, i, App(male, newRef)))
      case Female => 
        addReferent(newRef, Abs(newRef, i, App(female, newRef)))
      case Other  => 
        addReferent(newRef, Abs(newRef, i, App(nonHuman, newRef)))
    }
  def deleteReferent(oldRef: Sym): Context =
    copy(referentProperties = referentProperties - oldRef)
  def addBoundSym(sym: Sym): Context =
    copy(boundSymbols = boundSymbols + sym)
}
