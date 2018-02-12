package sensala.structure

import org.aossie.scavenger.expression.{E, Sym}
import org.atnos.eff.Eff

trait NL {
  def interpret(cont: NLEff[E]): NLEff[E]
}

trait Word extends NL {
  val word: String

  override def interpret(cont: NLEff[E]): NLEff[E] = Eff.pure(Sym(word))
}
