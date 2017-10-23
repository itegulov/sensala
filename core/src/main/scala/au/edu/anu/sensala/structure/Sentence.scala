package au.edu.anu.sensala.structure

import org.aossie.scavenger.expression._

case class Sentence(subjectPhrase: NounPhrase, verbPhrase: VerbPhrase) extends NL {
  def interpret: CState =
    for {
      subjectL <- subjectPhrase.interpret
      verbL    <- verbPhrase.interpret
    } yield App(verbL, subjectL)
}
