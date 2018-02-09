package sensala.structure.wh

import org.aossie.scavenger.expression._
import sensala.structure.noun.{NounPhrase, NounPhraseWithoutVerbPhrase}
import sensala.structure.verb.VerbPhrase
import sensala.structure._

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhraseWithoutVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- getEntity
      nounL <- nounPhrase.interpret(
        verbPhrase.interpret(
          for {
            _ <- putEntity(x) // Because who clause can redefine current entity
            result <- cont
          } yield result
        )
      )
    } yield nounL

  override def properties = nounPhrase.properties
}
