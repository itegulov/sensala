package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait VerbPhrase extends NL

final case class IntransitiveVerb(
  word: String
) extends Word with VerbPhrase {
  override def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(x, i, And(App(w, x), App(cont, x)))
}

final case class VerbObjPhrase(
  word: String,
  obj: NounPhraseWithoutVerbPhrase
) extends VerbPhrase {
  def interpret(cont: E): CState =
    for {
      x <- bindFreeVar
      y <- bindFreeVar
      w = Sym(word)
      objL  <- obj.interpret(Abs(y, i, And(AppRec(w, List(x, y)), App(cont, x))))
    } yield Abs(x, i, objL)
}

final case class VerbSentencePhrase(
  word: String,
  sentence: NounPhraseWithVerbPhrase
) extends VerbPhrase {
  override def interpret(cont: E): CState = for {
    // TODO: probably I should use the verb somehow
    sentenceL <- sentence.interpret(cont)
    x <- bindFreeVar
  } yield Abs(x, i, sentenceL)
}

final case class VerbAdjectivePhrase(
  verb: String,
  adjective: Adjective
) extends VerbPhrase {
  override def interpret(cont: E): CState =
    verb match {
      case "is" | "was" =>
        for {
          x <- bindFreeVar
          w = Sym(adjective.word)
        } yield Abs(x, i, And(App(w, x), App(cont, x)))
      case _ => ???
    }
}
