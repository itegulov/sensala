package au.edu.anu.sensala.postprocessing

import org.aossie.scavenger.expression.E
import org.aossie.scavenger.expression.formula._

object PrettyTransformer {
  def moveNegIn(f: E): E =
    f match {
      case Neg(And(Neg(a), Neg(b))) => Or(moveNegIn(a), moveNegIn(b))
      case Neg(And(a, b))           => Imp(moveNegIn(a), moveNegIn(Neg(b)))
      case Neg(Or(a, b))            => And(moveNegIn(Neg(a)), moveNegIn(Neg(b)))
      case Neg(Ex(v, t, g))         => All(v, t, moveNegIn(Neg(g)))
      case Neg(All(v, t, g))        => Ex(v, t, moveNegIn(Neg(g)))
      case Neg(Neg(g))              => moveNegIn(g)
      case And(a, b)                => And(moveNegIn(a), moveNegIn(b))
      case Or(a, b)                 => Or(moveNegIn(a), moveNegIn(b))
      case All(v, t, g)             => All(v, t, moveNegIn(g))
      case Ex(v, t, g)              => Ex(v, t, moveNegIn(g))
      case _                        => f
    }

  def removeMeaninglessAnd(f: E): E =
    f match {
      case And(a, True) => removeMeaninglessAnd(a)
      case And(True, a) => removeMeaninglessAnd(a)
      case Neg(a)       => Neg(removeMeaninglessAnd(a))
      case And(a, b)    => And(removeMeaninglessAnd(a), removeMeaninglessAnd(b))
      case Imp(a, b)    => Imp(removeMeaninglessAnd(a), removeMeaninglessAnd(b))
      case Or(a, b)     => Or(removeMeaninglessAnd(a), removeMeaninglessAnd(b))
      case All(v, t, g) => All(v, t, removeMeaninglessAnd(g))
      case Ex(v, t, g)  => Ex(v, t, removeMeaninglessAnd(g))
      case _            => f
    }

  def transform(f: E): E = removeMeaninglessAnd(moveNegIn(f))
}
