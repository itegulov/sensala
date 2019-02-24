package sensala.structure.wh

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import sensala.property.Property
import sensala.structure.noun.NounPhrase
import sensala.structure.verb.VerbPhrase
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}

final case class WhNounPhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  verbPhrase: VerbPhrase[F],
  nounPhrase: NounPhrase[F]
) extends WhPhrase[F]
    with NounPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    nounPhrase.interpret(
      for {
        x <- LocalContext[F].getEntity
        vpL <- verbPhrase.interpret(
                for {
                  _ <- LocalContext[F]
                        .putEntity(x) // Because who clause can redefine current entity
                  contL <- cont
                } yield contL
              )
      } yield vpL
    )

  override def properties                         = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
