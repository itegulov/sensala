package sensala.structure.adjective

import cats.data.State
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import org.atnos.eff.Eff
import sensala.structure.noun.{NounPhrase, NounPhraseWithVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._

trait AdjectiveWithVerbPhrase extends AdjectivePhrase with NounPhraseWithVerbPhrase

final case class AdjectiveNounPhraseVP(
  adjective: Adjective,
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends AdjectiveWithVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Eff.pure(Abs(y, i, And(App(w, y), App(verbL, y)))))
    } yield Abs(x, i, App(nounL, x))

  override def properties = nounPhrase.properties
}
