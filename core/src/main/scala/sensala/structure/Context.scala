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
  entityProperties: Map[Var, E],
  eventProperties: Map[Var, E],
  boundVars: Set[Var]
) {
  private val clausifier = new TPTPClausifier()

  def findAnaphoricEntity(v: Var, properties: E): Option[Var] =
    entityProperties.find {
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
  def findAnaphoricEvent(v: Var, properties: E): Option[Var] =
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
  def addEntity(newRef: Var, properties: List[Property]): Context =
    copy(
      entityProperties = entityProperties.updated(
        newRef,
        All(newRef, entity, properties.map(p => p.propertyExp(newRef)).reduceLeft(And.apply))
      )
    )
  def addEvent(newEvent: Var, properties: List[Property]): Context =
    copy(
      eventProperties = eventProperties.updated(
        newEvent,
        All(newEvent, event, properties.map(p => p.propertyExp(newEvent)).reduceLeft(And.apply))
      )
    )
  def addEvent(newEvent: Var, properties: E): Context =
    copy(
      eventProperties = eventProperties.updated(
        newEvent,
        All(newEvent, event, properties)
      )
    )
  def deleteReferent(oldRef: Var): Context =
    copy(entityProperties = entityProperties - oldRef)
  def addBoundVar(v: Var): Context =
    copy(boundVars = boundVars + v)
}
