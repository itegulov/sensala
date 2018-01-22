package sensala.structure.verb

import org.aossie.scavenger.expression.formula.And
import org.aossie.scavenger.expression._
import sensala.structure.adjective.Adjective
import sensala.structure._
import org.atnos.eff.all._

final case class VerbAdjectivePhrase(
  verb: String,
  adjective: Adjective
) extends VerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    verb match {
      case "is" | "was" =>
        for {
          x <- bindFreeVar
          w = Sym(adjective.word)
          contL <- cont
        } yield Abs(x, i, And(App(w, x), App(contL, x)))
      case _ =>
        left[NLFx, String, E]("Unknown adjective verb")
    }
}
