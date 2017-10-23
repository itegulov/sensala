package au.edu.anu.sensala.structure

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula.True

case class Sentence(subjectPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NL {
  def interpret: CState =
    for {
      subjectL <- subjectPhrase.interpret
      verbL    <- verbPhrase.interpret
    } yield App(App(verbL, subjectL), True)
}
