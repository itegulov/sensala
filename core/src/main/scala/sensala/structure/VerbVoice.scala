package sensala.structure

sealed trait VerbVoice
case object Active  extends VerbVoice
case object Passive extends VerbVoice
