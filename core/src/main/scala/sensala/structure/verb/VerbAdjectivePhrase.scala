package sensala.structure.verb

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import sensala.structure.adjective.Adjective
import sensala.structure.{CState, bindFreeVar}

final case class VerbAdjectivePhrase(
  verb: String,
  adjective: Adjective
) extends VerbPhrase {
  override def interpret(cont: E): CState =
    verb match {
      case "is" | "was" =>
        for {
          x <- bindFreeVar
          w = Sym(adjective.word)
        } yield Abs(x, i, And(App(w, x), App(cont, x)))
      case _ => ???
    }
}
