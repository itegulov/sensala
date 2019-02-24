package sensala.structure

import org.aossie.scavenger.expression.E
import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase

final case class Sentence[F[_]](nounPhrase: NounPhrase[F], verbPhrase: VerbPhrase[F])
    extends NL[F] {
  override def interpret(cont: F[E]): F[E] =
    nounPhrase.interpret(verbPhrase.interpret(cont))
}
