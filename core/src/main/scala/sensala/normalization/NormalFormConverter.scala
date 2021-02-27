package sensala.normalization

import org.aossie.scavenger.expression._

object NormalFormConverter {
  def substitute(l: E, v: String, e: E, scope: Set[String] = Set()): E =
    l match {
      case lamV: Sym if v == lamV.name => e
      case v: Sym                      => v
      case App(x, y) =>
        App(substitute(x, v, e, scope), substitute(y, v, e, scope))
      case t @ Abs(bind, typ, body) =>
        val bodyFree = body.freeVariables.map(_.name).toSet
        if (bind.name == v || !bodyFree.contains(v)) {
          t
        } else {
          Abs(bind, typ, substitute(body, v, e, Set()))
        }
    }

  def headNormalForm(l: E, scope: Set[String] = Set()): E =
    l match {
      case v: Sym               => v
      case Abs(bind, typ, body) => Abs(bind, typ, headNormalForm(body, scope + bind.name))
      case App(lhs, rhs) =>
        headNormalForm(lhs, scope) match {
          case Abs(bind, _, body) =>
            headNormalForm(
              substitute(body, bind.name, rhs, scope + bind.name),
              scope + bind.name
            )
          case c =>
            App(c, rhs)
        }
    }

  def normalForm(l: E, scope: Set[String] = Set()): E =
    l match {
      case v: Sym               => v
      case Abs(bind, typ, body) => Abs(bind, typ, normalForm(body, scope + bind.name))
      case App(lhs, rhs) =>
        headNormalForm(lhs) match {
          case Abs(bind, _, body) =>
            normalForm(substitute(body, bind.name, rhs, scope + bind.name), scope + bind.name)
          case o => App(normalForm(o, scope), normalForm(rhs, scope))
        }
    }
}
