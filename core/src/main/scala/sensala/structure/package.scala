package sensala

import cats.mtl.FunctorRaise
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.error.NLError
import sensala.structure.types._

package object structure {
  type FunctorRaiseNLError[F[_]] = FunctorRaise[F, NLError]

  object FunctorRaiseNLError {
    def apply[F[_]](implicit ev: FunctorRaiseNLError[F]): FunctorRaiseNLError[F] = ev
  }

  val agent       = Sym("agent")
  val patient     = Sym("patient")
  val description = Sym("description")

  val named = Sym("named")

  val animal       = Sym("animal")
  val location     = Sym("location")
  val person       = Sym("person")
  val organization = Sym("organization")
  val money        = Sym("money")
  val percent      = Sym("percent")
  val date         = Sym("date")
  val time         = Sym("time")

  val male   = Sym("male")
  val female = Sym("female")

  // Special Sensala properties
  val speaker      = Sym("sensala_speaker")
  val interlocutor = Sym("sensala_interlocutor")

  // FIXME: Make True a case object in Scavenger
  val Truth: E = True

  // FIXME: find a better way to represent truth and false
  def truth(x: Sym): E = person(x) \/ ~person(x)

  def substitute(exp: E, old: Sym, newE: E): E = exp match {
    case `old`                       => newE
    case s: Sym                      => s
    case Abs(v, t, body) if v != old => Abs(v, t, substitute(body, old, newE))
    case abs: Abs                    => abs
    case App(f, a)                   => App(substitute(f, old, newE), substitute(a, old, newE))
  }

  def substitute(exp: E, predicate: Sym, position: Int, newE: E): E = exp match {
    case AppRec(`predicate`, args) if position < args.size =>
      AppRec(predicate, args.toList.patch(position, List(newE), 1))
    case s: Sym =>
      s
    case Abs(v, t, body) =>
      Abs(v, t, substitute(body, predicate, position, newE))
    case App(f, a) =>
      App(substitute(f, predicate, position, newE), substitute(a, predicate, position, newE))
  }

  implicit class ERich(val lambda: E) extends AnyVal {
    def pretty: String =
      lambda match {
        case Neg(b)                      => s"¬${b.pretty}"
        case All(v, t, b) if t == entity => s"(∀${v.pretty}ᵉⁿ.${b.pretty})"
        case All(v, t, b) if t == event  => s"(∀${v.pretty}ᵉᵛ.${b.pretty})"
        case Ex(v, t, b) if t == entity  => s"(∃${v.pretty}ᵉⁿ.${b.pretty})"
        case Ex(v, t, b) if t == event   => s"(∃${v.pretty}ᵉᵛ.${b.pretty})"
        case And(left, right)            => s"${left.pretty} ∧ ${right.pretty}"
        case Or(left, right)             => s"${left.pretty} ∨ ${right.pretty}"
        case Imp(left, right)            => s"${left.pretty} → ${right.pretty}"
        case Sym(name)                   => name
        case AppRec(f, args)             => s"${f.pretty}(${args.map(_.pretty).mkString(", ")})"
        case App(f, a)                   => s"(${f.pretty}(${a.pretty}))"
        case Abs(v, _, e)                => s"(λ${v.pretty}.${e.pretty})"
      }

    def apply(args: E*): E = AppRec(lambda, args)

    def \/(right: E): E = Or(lambda, right)

    def /\(right: E): E = And(lambda, right)

    def ->:(right: E): E = Imp(right, lambda)

    def unary_~(): E = Neg(lambda)
  }
}
