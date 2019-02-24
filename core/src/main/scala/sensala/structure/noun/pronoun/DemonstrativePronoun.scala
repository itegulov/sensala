package sensala.structure.noun.pronoun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression.E
import sensala.property.Property
import sensala.structure.context.{Context, LocalContext}

final case class DemonstrativePronoun[F[_]: Monad: Context: LocalContext](
  word: String
) extends Pronoun[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      e     <- Context[F].findAnaphoricEventUnsafe(List.empty)
      _     <- LocalContext[F].putEntity(e)
      contL <- cont
    } yield contL

  override def properties: List[Property]         = List.empty
  override def definiteProperties: List[Property] = List.empty
}
