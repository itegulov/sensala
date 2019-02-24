package sensala.structure.noun

import cats.Monad
import cats.implicits._
import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.property.{Property, WordNetPropertyExtractor}
import sensala.structure.context.LocalContext

sealed trait NounPhraseBasic[F[_]] extends NounPhrase[F]

final case class ProperNoun[F[_]: Monad: LocalContext](
  word: String,
  typ: Option[NamedEntityType],
  gender: Option[NamedEntityGender]
) extends Word[F]
    with NounPhraseBasic[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- LocalContext[F].getEntity
      w     = Sym(word)
      contL <- cont
    } yield named(x, w) /\ contL

  private def typProperty: List[Property] = typ match {
    case Some(Location)     => List(Property(x => location(x)))
    case Some(Person)       => List(Property(x => person(x)))
    case Some(Organization) => List(Property(x => organization(x)))
    case Some(Money)        => List(Property(x => money(x)))
    case Some(Percent)      => List(Property(x => percent(x)))
    case Some(Date)         => List(Property(x => date(x)))
    case Some(Time)         => List(Property(x => time(x)))
    case None               => List()
  }

  private def genderProperty: List[Property] = gender match {
    case Some(Male)   => List(Property(x => male(x)))
    case Some(Female) => List(Property(x => female(x)))
    case None         => List()
  }

  override def properties: List[Property]         = typProperty ++ genderProperty
  override def definiteProperties: List[Property] = properties
}

final case class CommonNoun[F[_]: Monad: LocalContext](
  word: String
) extends Word[F]
    with NounPhraseBasic[F] {
  override def interpret(cont: F[E]): F[E] =
    for {
      x     <- LocalContext[F].getEntity
      w     = Sym(word)
      contL <- cont
    } yield w(x) /\ contL

  override def properties: List[Property]         = WordNetPropertyExtractor.extractProperties(word)
  override def definiteProperties: List[Property] = List(Property(x => Sym(word)(x)))
}
