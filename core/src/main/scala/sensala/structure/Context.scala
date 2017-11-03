package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{All, And, Neg}
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.prover.{EPCR, Unsatisfiable}
import org.aossie.scavenger.structure.immutable.{AxiomClause, NegConjectureClause}
import sensala.property.Property

import scala.concurrent.duration._

final case class Context(
  referentProperties: Map[Sym, E],
  boundSymbols: Set[Sym]
) {
  private val clausifier = new TPTPClausifier()
  
  def findAnaphoricReferent(v: Var, properties: E): Option[Sym] =
    referentProperties.find {
      case (_, refProperties) =>
        val cnf = clausifier(List((refProperties, AxiomClause), (All(v, i, Neg(properties)), NegConjectureClause)))
        EPCR.prove(cnf, 5 seconds) match {
          case Unsatisfiable(_) => true
          case _ => false
        }
    }.map(_._1)
  def addReferent(newRef: Var, properties: List[Property]): Context =
    copy(referentProperties = 
      referentProperties.updated(
        newRef, 
        All(newRef, i, properties.map(p => App(p.symbol, newRef)).reduceLeft(And.apply))
      )
    )
  def deleteReferent(oldRef: Sym): Context =
    copy(referentProperties = referentProperties - oldRef)
  def addBoundSym(sym: Sym): Context =
    copy(boundSymbols = boundSymbols + sym)
}
