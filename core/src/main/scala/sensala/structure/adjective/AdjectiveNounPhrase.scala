package sensala.structure.adjective

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import sensala.property.Property
import sensala.structure.noun.NounPhrase
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}

final case class AdjectiveNounPhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  adjective: Adjective,
  nounPhrase: NounPhrase[F]
) extends AdjectivePhrase[F] with NounPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    nounPhrase.interpret(
      for {
        y     <- LocalContext[F].getEntity
        w     = Sym(adjective.word)
        contL <- cont
      } yield w(y) /\ contL
    )

  override def properties = nounPhrase.properties
  override def definiteProperties: List[Property] = nounPhrase.definiteProperties
}
