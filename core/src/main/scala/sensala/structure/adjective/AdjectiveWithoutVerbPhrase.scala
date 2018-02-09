package sensala.structure.adjective

import org.aossie.scavenger.expression._
import org.atnos.eff._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure._
import sensala.structure.types._

trait AdjectiveWithoutVerbPhrase extends AdjectivePhrase with NounPhraseWithoutVerbPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      contL <- cont
      nounL <- nounPhrase.interpret(Eff.pure(Abs(y, entity, w(y) /\: contL(y))))
    } yield Abs(x, entity, nounL(x))

  override def properties = nounPhrase.properties
}
