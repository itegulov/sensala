package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure.adjective.Adjective
import sensala.structure._
import sensala.error.NLUnexpectedWord
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types.event

final case class VerbAdjectivePhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  verb: String,
  adjective: Adjective
) extends VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    verb match {
      case "is" | "was" =>
        for {
          x     <- LocalContext[F].getEntity
          e     <- Context[F].bindFreeVar
          _     <- LocalContext[F].putEvent(e)
          w     = Sym(adjective.word)
          _     <- Context[F].addEvent(e, description(e) /\ w(e, x))
          contL <- cont
        } yield Ex(e, event, description(e) /\ w(e, x) /\ contL)
      case other =>
        FunctorRaiseNLError[F].raise(NLUnexpectedWord(other))
    }
}
