package sensala.structure

sealed trait PronounGender

case object Masculine extends PronounGender
case object Feminine  extends PronounGender
case object Neuter    extends PronounGender
