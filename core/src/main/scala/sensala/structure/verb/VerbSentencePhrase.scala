package sensala.structure.verb

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.Ex
import sensala.structure._
import sensala.structure.context.{Context, LocalContext}
import sensala.structure.types.event

final case class VerbSentencePhrase[F[_]: Monad: Context: LocalContext: FunctorRaiseNLError](
  word: String,
  sentence: Sentence[F]
) extends VerbPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      e <- Context[F].bindFreeVar
      x <- LocalContext[F].getEntity
      _ <- LocalContext[F].putEvent(e)
      w = Sym(word)
      sentenceL <- sentence.interpret(
                    for {
                      eSucc <- LocalContext[F].getEvent
                      _     <- Context[F].addEvent(e, w(e) /\ agent(e, x) /\ patient(e, eSucc))
                      contL <- cont
                    } yield w(e) /\ agent(e, x) /\ patient(e, eSucc) /\ contL
                  )
    } yield Ex(e, event, sentenceL)
}
