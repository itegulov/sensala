package au.edu.anu.sensala.normalization

import au.edu.anu.sensala.structure._

import scala.collection.mutable

object NormalFormConverter {
  private case class Redex(v: String, body: L, arg: L)

  private val cache         = mutable.WeakHashMap.empty[Redex, L]
  private val variableCache = mutable.Map.empty[String, Sym]

  def substitute(l: L, v: String, e: L, scope: Set[String] = Set()): L = l match {
    case lamV: Sym if v == lamV.name => e
    case v: Sym                      => v
    case App(x, y) =>
      App(substitute(x, v, e, scope), substitute(y, v, e, scope))
    case t @ Abs(bind, body) =>
      val bodyFree = body.freeSymbols.map(_.name)
      if (bind.name == v || !bodyFree.contains(v)) {
        t
      } else {
        val free = getFreeName(bodyFree ++ e.freeSymbols.map(_.name) ++ scope)
        val freeVar = variableCache.get(free) match {
          case Some(oldV) => oldV
          case None =>
            val freeVar = Sym(free)
            variableCache(free) = freeVar
            freeVar
        }
        val sBody = substitute(
          substitute(body, bind.name, freeVar, scope + free),
          v,
          e,
          scope + free
        )
        if ((freeVar eq bind) && (sBody eq body)) l else Abs(freeVar, sBody)
      }
  }

  def getFreeName(used: Set[String], range: Seq[String] = ('a' to 'z').map(_.toString)): String =
    range.find(c => !used.contains(c)) match {
      case Some(c) => c
      case _       => getFreeName(used, range.map(_ + "'"))
    }

  def headNormalForm(l: L, scope: Set[String] = Set()): L = l match {
    case v: Sym          => v
    case Abs(bind, body) => Abs(bind, headNormalForm(body, scope + bind.name))
    case App(lhs, rhs) =>
      headNormalForm(lhs, scope) match {
        case Abs(bind, body) =>
          val redex = Redex(bind.name, body, rhs)
          cache.get(redex) match {
            case Some(r) => r
            case None =>
              val hnf = headNormalForm(
                substitute(body, bind.name, rhs, scope + bind.name),
                scope + bind.name
              )
              cache(redex) = hnf
              hnf
          }
        case c => App(c, rhs)
      }
  }

  def normalForm(l: L, scope: Set[String] = Set()): L = {
    l match {
      case v: Sym => v
      case Abs(bind, body) => Abs(bind, normalForm(body, scope + bind.name))
      case App(lhs, rhs) =>
        headNormalForm(lhs) match {
          case Abs(bind, body) =>
            normalForm(substitute(body, bind.name, rhs, scope + bind.name), scope + bind.name)
          case o => App(normalForm(o, scope), normalForm(rhs, scope))
        }
    }
  }
}
