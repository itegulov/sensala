package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.property.{Property, WordNetPropertyExtractor}

sealed trait NounPhraseBasic extends NounPhrase

final case class ProperNoun(
  word: String,
  typ: Option[NamedEntityType],
  gender: Option[NamedEntityGender]
) extends Word
    with NounPhraseBasic {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      contL <- cont
    } yield named(x, w) /\: contL

  private def typProperty: List[Property] = typ match {
    case Some(Location)     => List(Property(location))
    case Some(Person)       => List(Property(person))
    case Some(Organization) => List(Property(organization))
    case Some(Money)        => List(Property(money))
    case Some(Percent)      => List(Property(percent))
    case Some(Date)         => List(Property(date))
    case Some(Time)         => List(Property(time))
    case None               => List()
  }

  private def genderProperty: List[Property] = gender match {
    case Some(Male)   => List(Property(male))
    case Some(Female) => List(Property(female))
    case None         => List()
  }

  override def properties: List[Property] =
    typProperty ++ genderProperty
}

final case class CommonNoun(
  word: String
) extends Word
    with NounPhraseBasic {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      contL <- cont
    } yield w(x) /\: contL

  override def properties: List[Property] = WordNetPropertyExtractor.extractProperties(word)
}
