package sensala.structure.verb

import org.aossie.scavenger.expression.{E, Sym}
import sensala.structure.adverb.AdverbPhrase
import sensala.structure._

case class VerbAdverbPhrase(adverb: String, verbPhrase: VerbPhrase) extends AdverbPhrase with VerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    verbPhrase.interpret(
      for {
        e <- getEvent
        w = Sym(adverb)
        contL <- cont
      } yield w(e) /\: contL
    )
}
