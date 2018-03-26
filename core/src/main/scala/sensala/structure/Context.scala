package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.{All, And}
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.prover.{EPCR, Unsatisfiable}
import org.aossie.scavenger.structure.immutable.{AxiomClause, NegConjectureClause}
import sensala.property.Property
import sensala.structure.types.{entity, event}

import scala.concurrent.duration._

final case class Context(
  referentProperties: Map[Sym, E],
  eventProperties: Map[Sym, E],
  boundSymbols: Set[Sym]
) {
  private val clausifier = new TPTPClausifier()

  def findAnaphoricReferent(v: Var, properties: E): Option[Sym] =
    referentProperties.find {
      case (_, refProperties) =>
        val cnf = clausifier(
          List(
            (refProperties, AxiomClause),
            (All(v, entity, ~properties), NegConjectureClause)
          )
        )
        EPCR.prove(cnf, 5 seconds) match {
          case Unsatisfiable(_) => true
          case _                => false
        }
    }.map(_._1)
  def findAnaphoricEvent(v: Var, properties: E): Option[Sym] =
    eventProperties.find {
      case (_, evProperties) =>
        val cnf = clausifier(
          List(
            (evProperties, AxiomClause),
            (All(v, event, ~properties), NegConjectureClause)
          )
        )
        EPCR.prove(cnf, 5 seconds) match {
          case Unsatisfiable(_) => true
          case _                => false
        }
    }.map(_._1)
  def addReferent(newRef: Var, properties: List[Property]): Context =
    copy(
      referentProperties = referentProperties.updated(
        newRef,
        All(newRef, entity, properties.map(p => App(p.symbol, newRef)).reduceLeft(And.apply))
      )
    )
  def addEvent(newEvent: Var, properties: List[Property]): Context =
    copy(
      eventProperties = eventProperties.updated(
        newEvent,
        All(newEvent, event, properties.map(p => App(p.symbol, newEvent)).reduceLeft(And.apply))
      )
    )
  def addEvent(newEvent: Var, properties: E): Context =
    copy(
      eventProperties = eventProperties.updated(
        newEvent,
        All(newEvent, event, properties)
      )
    )
  def deleteReferent(oldRef: Sym): Context =
    copy(referentProperties = referentProperties - oldRef)
  def addBoundSym(sym: Sym): Context =
    copy(boundSymbols = boundSymbols + sym)
}
