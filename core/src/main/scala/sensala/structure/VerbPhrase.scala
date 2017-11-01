package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait VerbPhrase extends NL

case class IntransitiveVerb(word: String) extends Word with VerbPhrase {
  override def interpret =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      w = Sym(word)
    } yield Abs(f, i -> o, Abs(x, i, And(App(w, x), App(f, x))))
}

case class VerbObjPhrase(word: String, obj: NounPhraseWithoutVerbPhrase) extends VerbPhrase {
  def interpret: CState =
    for {
      f <- bindFreeVar
      x <- bindFreeVar
      y <- bindFreeVar
      objL  <- obj.interpret
      w = Sym(word)
    } yield Abs(f, i -> o, Abs(x, i, App(objL, Abs(y, i, And(AppRec(w, List(x, y)), App(f, x))))))
}

case class VerbSentencePhrase(word: String, sentence: NounPhraseWithVerbPhrase) extends VerbPhrase {
  override def interpret: CState = for {
    // TODO: probably I should use the verb somehow
    sentenceL <- sentence.interpret
    f <- bindFreeVar
    x <- bindFreeVar
    y <- bindFreeVar
  } yield Abs(f, i -> o, Abs(x, i, App(sentenceL, f)))
}

case class VerbAdjectivePhrase(verb: String, adjective: Adjective) extends VerbPhrase {
  override def interpret: CState =
    verb match {
      case "is" | "was" =>
        for {
          f <- bindFreeVar
          x <- bindFreeVar
          w = Sym(adjective.word)
        } yield Abs(f, i -> o, Abs(x, i, And(App(w, x), App(f, x))))
      case _ => ???
    }
}
