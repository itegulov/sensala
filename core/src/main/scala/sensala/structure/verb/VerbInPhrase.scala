package sensala.structure.verb

import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.structure.prepositional.PrepositionalPhrase

case class VerbInPhrase(propositionalPhrase: PrepositionalPhrase, verbPhrase: VerbPhrase)
    extends VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verbPhrase.interpret(
      for {
        e <- getEvent
        locationL <- propositionalPhrase.nounPhrase.interpret(
                      for {
                        x     <- getEntity
                        w     = Sym(propositionalPhrase.word)
                        contL <- cont
                      } yield w(e, x) /\: contL
                    )
      } yield locationL
    )
}
