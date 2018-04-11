package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.property.Property
import sensala.structure.verb.VerbPhrase
import sensala.structure._

trait NounPhrase extends NL {
  def properties: List[Property]
}

final case class NounPhraseWithVerbPhrase(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NounPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] = nounPhrase.interpret(verbPhrase.interpret(cont))

  override def properties: List[Property] = nounPhrase.properties
}
