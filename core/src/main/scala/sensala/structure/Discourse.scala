package sensala.structure

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types._

final case class Discourse[F[_]: Monad: Context: LocalContext](sentences: List[Sentence[F]])
    extends NL[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x <- Context[F].bindFreeVar
      result <- sentences.foldLeftM[F, E](Abs(x, i, x)) {
                 case (e, b) =>
                   for {
                     z      <- Context[F].bindFreeVar
                     _      <- LocalContext[F].clear
                     intRes <- b.interpret(Monad[F].pure[E](z))
                   } yield Abs(z, entity, e(intRes))
               }
      contL <- cont
    } yield result(contL)
}
