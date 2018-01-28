package sensala.structure.adjective

import org.aossie.scavenger.expression._
import org.atnos.eff._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure._

trait AdjectiveWithoutVerbPhrase extends AdjectivePhrase with NounPhraseWithoutVerbPhrase

final case class AdjectiveNounPhrase(
  adjective: Adjective,
  nounPhrase: NounPhrase
) extends AdjectiveWithoutVerbPhrase {
  override def interpret(cont: NLEffE): NLEffE =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(adjective.word)
      contL <- cont
      nounL <- nounPhrase.interpret(Eff.pure(Abs(y, i, w(y) /\ contL(y))))
    } yield Abs(x, i, nounL(x))

  override def properties = nounPhrase.properties
}
