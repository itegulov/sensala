package sensala.structure.adjective

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And
import sensala.structure.noun.{NounPhrase, NounPhraseWithVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._

trait AdjectiveWithVerbPhrase extends AdjectivePhrase with NounPhraseWithVerbPhrase

final case class AdjectiveNounPhraseVP(
  adjective: Adjective,
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends AdjectiveWithVerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Abs(y, i, And(App(w, y), App(verbL, y))))
    } yield Abs(x, i, App(nounL, x))

  override def properties = nounPhrase.properties
}
