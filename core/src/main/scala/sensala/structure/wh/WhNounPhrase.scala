package sensala.structure.wh

import org.aossie.scavenger.expression._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._
import sensala.structure.types._

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      nounL <- nounPhrase.interpret(
                for {
                  verbL <- verbPhrase.interpret(cont)
                } yield Abs(y, entity, verbL(y))
              )
    } yield Abs(x, entity, nounL(x))

  override def properties = nounPhrase.properties
}
