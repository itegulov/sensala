package au.edu.anu.sensala.normalization

import au.edu.anu.sensala.structure._
import cats.data.State

object NormalFormConverter {
  def substitute(l: L, v: Sym, e: L): CState = l match {
    case lamV: Sym if v == lamV =>
      State { context =>
        if (context.referents.contains(v)) {
          e match {
            case s: Sym => (context.addConversion(v, s).deleteReferent(v).extend(s), e)
            case _      => (context, e)
          }
        } else {
          (context, e)
        }
      }
    case v: Sym => State.pure(v)
    case App(x, y) =>
      for {
        substX <- substitute(x, v, e)
        substY <- substitute(y, v, e)
      } yield App(substX, substY)
    case t @ Abs(bind, body) =>
      val bodyFree = body.freeSymbols
      if (bind == v || !bodyFree.contains(v)) {
        State.pure(t)
      } else {
        substitute(body, v, e).map(Abs(bind, _))
      }
  }

  def headNormalForm(l: L): CState = l match {
    case v: Sym          => State.pure(v)
    case Abs(bind, body) => headNormalForm(body).map(Abs(bind, _))
    case App(lhs, rhs) =>
      headNormalForm(lhs).flatMap {
        case Abs(bind, body) =>
          for {
            substL <- substitute(body, bind, rhs)
            hnfL   <- headNormalForm(substL)
          } yield hnfL
        case c => State.pure(App(c, rhs))
      }
  }

  def normalForm(l: L): CState =
    l match {
      case v: Sym          => State.pure(v)
      case Abs(bind, body) => normalForm(body).map(Abs(bind, _))
      case App(lhs, rhs) =>
        headNormalForm(lhs).flatMap {
          case Abs(bind, body) =>
            for {
              substL <- substitute(body, bind, rhs)
              result <- normalForm(substL)
            } yield result
          case o =>
            for {
              oL   <- normalForm(o)
              rhsL <- normalForm(rhs)
            } yield App(oL, rhsL)
        }
    }
}
