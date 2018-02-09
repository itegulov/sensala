package sensala.structure.adjective

import org.aossie.scavenger.expression._
import org.atnos.eff.Eff
import sensala.structure.noun.{NounPhrase, NounPhraseWithVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._
import sensala.structure.types._

trait AdjectiveWithVerbPhrase extends AdjectivePhrase with NounPhraseWithVerbPhrase

final case class AdjectiveNounPhraseVP(
  adjective: Adjective,
  nounPhrase: NounPhrase,
  verbPhrase: VerbPhrase
) extends AdjectiveWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      verbL <- verbPhrase.interpret(cont)
      nounL <- nounPhrase.interpret(Eff.pure(Abs(y, entity, w(y) /\: verbL(y))))
    } yield Abs(x, entity, nounL(x))

  override def properties = nounPhrase.properties
}
