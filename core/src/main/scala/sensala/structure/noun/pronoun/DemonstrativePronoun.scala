package sensala.structure.noun.pronoun

import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure._

final case class DemonstrativePronoun(
  word: String
) extends Pronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      e     <- findAnaphoricEvent(List.empty)
      _     <- putEntity(e)
      contL <- cont
    } yield contL

  override def properties: List[Property]         = List.empty
  override def definiteProperties: List[Property] = List.empty
}
