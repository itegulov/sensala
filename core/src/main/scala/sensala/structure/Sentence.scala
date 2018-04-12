package sensala.structure

import org.aossie.scavenger.expression.E
import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase

final case class Sentence(nounPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NL {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    nounPhrase.interpret(verbPhrase.interpret(cont))
}
