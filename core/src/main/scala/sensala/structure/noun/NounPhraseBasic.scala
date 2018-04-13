package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import sensala.property.{Property, WordNetPropertyExtractor}

sealed trait NounPhraseBasic extends NounPhrase

final case class ProperNoun(
  word: String
) extends Word
    with NounPhraseBasic {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      contL <- cont
    } yield named(x, w) /\: contL

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female), Property(person))
    case "John" => List(Property(male), Property(person))
    case _      => List(Property(animal))
  }
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
