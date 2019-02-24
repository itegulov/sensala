package sensala.structure.noun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import sensala.property.Property
import sensala.structure._
import sensala.structure.context.LocalContext
import sensala.structure.prepositional.PrepositionalPhrase

final case class NounPhrasePreposition[F[_]: Monad: LocalContext](
  prepositionalPhrase: PrepositionalPhrase[F],
  nounPhrase: NounPhrase[F]
) extends NounPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    nounPhrase.interpret {
      for {
        e <- LocalContext[F].getEntity
        preposition <- prepositionalPhrase.nounPhrase.interpret {
                        for {
                          prepEntity <- LocalContext[F].getEntity
                          w          = Sym(prepositionalPhrase.word)
                          _          <- LocalContext[F].putEntity(e)
                          contL      <- cont
                        } yield w(prepEntity, e) /\ contL
                      }
      } yield preposition
    }

  // TODO: Add preposition property
  override def properties: List[Property]         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
