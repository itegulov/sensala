package sensala.structure.prepositional

import cats.Monad
import org.aossie.scavenger.expression.{E, Sym}
import sensala.structure.Word
import sensala.structure.noun.NounPhrase

sealed trait PrepositionalPhrase[F[_]] extends Word[F] {
  val nounPhrase: NounPhrase[F]
}

final case class InPhrase[F[_]: Monad](word: String, nounPhrase: NounPhrase[F]) extends PrepositionalPhrase[F] {
  override def interpret(cont: F[E]): F[E] =
    Monad[F].pure[E](Sym(word))
}

final case class PossessionPhrase[F[_]: Monad](nounPhrase: NounPhrase[F]) extends PrepositionalPhrase[F] {
  override val word: String = "owns"

  override def interpret(cont: F[E]): F[E] =
    Monad[F].pure[E](Sym(word))
}
