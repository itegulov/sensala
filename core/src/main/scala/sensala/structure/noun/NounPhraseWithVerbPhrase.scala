package sensala.structure.noun

import org.aossie.scavenger.expression._
import sensala.structure._
import org.atnos.eff.all._
import sensala.error.{NLError, NLUnexpectedWord}
import sensala.property.Property
import sensala.structure.verb.VerbPhrase

trait NounPhraseWithVerbPhrase extends NounPhrase {
  val verbPhrase: VerbPhrase
}

final case class ProperNounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word
    with NounPhraseWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x     <- getEntity
      w     = Sym(word)
      verbL <- verbPhrase.interpret(cont)
    } yield named(x, w) /\: verbL

  override def properties: List[Property] = word match {
    case "Mary" => List(Property(female))
    case "John" => List(Property(male))
    case _      => List(Property(animal))
  }
}

final case class ReflexivePronounVP(
  word: String,
  verbPhrase: VerbPhrase
) extends Word
    with NounPhraseWithVerbPhrase {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      ref <- PronounPropertyMap.map.get(word.toLowerCase) match {
              case Some(property) =>
                gets[NLFx, Context, E](_.findAnaphoricReferent(x, property(x)).get)
              case None => left[NLFx, NLError, E](NLUnexpectedWord(word))
            }
      _     <- putEntity(ref.asInstanceOf[Var]) // FIXME: remove type casting?
      verbL <- verbPhrase.interpret(cont)
    } yield verbL

  override def properties: List[Property] =
    PronounPropertyMap.map
      .get(word.toLowerCase)
      .map(sym => List(Property(sym)))
      .getOrElse(List(Property(animal)))
}
