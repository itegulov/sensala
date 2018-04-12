package sensala.structure.wh

import org.aossie.scavenger.expression._
import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase
import sensala.structure._

final case class WhNounPhrase(
  verbPhrase: VerbPhrase,
  nounPhrase: NounPhrase
) extends WhPhrase
    with NounPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret(
      for {
        x <- getEntity
        vpL <- verbPhrase.interpret(
          for {
            _     <- putEntity(x) // Because who clause can redefine current entity
            contL <- cont
          } yield contL
        )
      } yield vpL
    )

  override def properties = nounPhrase.properties
}
