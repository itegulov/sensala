package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import sensala.structure._
import sensala.structure.types._
import sensala.structure.context.{Context, LocalContext}

final case class IntransitiveVerb[F[_]: Monad: Context: LocalContext](
  word: String
) extends Word[F]
    with VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- LocalContext[F].getEntity
      e     <- Context[F].bindFreeVar
      _     <- LocalContext[F].putEvent(e)
      w     = Sym(word)
      _     <- Context[F].addEvent(e, w(e) /\ agent(e, x))
      contL <- cont
    } yield Ex(e, event, w(e) /\ agent(e, x) /\ contL)
}
