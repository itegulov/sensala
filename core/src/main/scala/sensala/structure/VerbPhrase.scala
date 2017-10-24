package sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.And

trait VerbPhrase extends NL

case class TransitiveVerb(word: String) extends Word {
  override def interpret: CState =
    for {
      f <- bindFreeSym
      x <- bindFreeSym
      y <- bindFreeSym
      p <- bindFreeSym
      q <- bindFreeSym
    } yield Abs(p, i, Abs(q, i, App(q, Abs(x, i, App(p, Abs(y, i, Abs(f, i, And(App(App(Sym(word), x), y), f))))))))
}

case class IntransitiveVerb(word: String) extends Word with VerbPhrase {
  override def interpret =
    for {
      f <- bindFreeSym
      p <- bindFreeSym
      q <- bindFreeSym
      x <- bindFreeSym
      y <- bindFreeSym
    } yield Abs(p, i, Abs(f, i, And(App(p, Abs(x, i, App(Sym(word), x))), f)))
}

case class VerbObjPhrase(verb: TransitiveVerb, obj: NounPhrase) extends VerbPhrase {
  def interpret: CState =
    for {
      verbL <- verb.interpret
      objL  <- obj.interpret
    } yield App(verbL, objL)
}

case class VerbSentencePhrase(verb: TransitiveVerb, sentence: Sentence) extends VerbPhrase {
  override def interpret: CState = ???
}

case class VerbAdjectivePhrase(verb: TransitiveVerb, adjective: Adjective) extends VerbPhrase {
  override def interpret: CState = ???
}
