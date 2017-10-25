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
      x <- bindFreeSym
      w = Sym(word)
    } yield Abs(p, i, App(p, Abs(x, i, Abs(f, i, And(App(w, x), f)))))
}

case class VerbObjPhrase(verb: TransitiveVerb, obj: NounPhrase) extends VerbPhrase {
  def interpret: CState =
    for {
      verbL <- verb.interpret
      objL  <- obj.interpret
    } yield App(verbL, objL)
}

case class VerbSentencePhrase(verb: TransitiveVerb, sentence: Sentence) extends VerbPhrase {
  override def interpret: CState =
    for {
      verbL <- verb.interpret // TODO: probably I should use this verb somehow
      senL <- sentence.interpret
      p <- bindFreeSym
      x <- bindFreeSym
      f <- bindFreeSym
    } yield Abs(p, i, App(p, Abs(x, i, Abs(f, i, App(senL, f)))))
}

case class VerbAdjectivePhrase(verb: TransitiveVerb, adjective: Adjective) extends VerbPhrase {
  override def interpret: CState =
    verb match {
      case TransitiveVerb("is") =>
        for {
          adjL <- adjective.interpret
          p <- bindFreeSym
          x <- bindFreeSym
          y <- bindFreeSym
        } yield Abs(p, i, App(p, App(adjL, Abs(x, i, Abs(y, i, y)))))
      case _ => ???
    }
}
