package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.noun.NounPhrase
import sensala.structure.types._

final case class TransitiveVerb[F[_]: Monad: Context: LocalContext](
  word: String,
  obj: NounPhrase[F]
) extends VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x <- LocalContext[F].getEntity
      e <- Context[F].bindFreeVar
      _ <- LocalContext[F].putEvent(e)
      w = Sym(word)
      objL <- obj.interpret(
               for {
                 y     <- LocalContext[F].getEntity
                 _     <- Context[F].addEvent(e, w(e) /\ agent(e, x) /\ patient(e, y))
                 contL <- cont
               } yield Ex(e, event, w(e) /\ agent(e, x) /\ patient(e, y) /\ contL)
             )
    } yield objL
}
