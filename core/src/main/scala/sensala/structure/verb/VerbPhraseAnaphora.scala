package sensala.structure.verb

sealed trait Voice
case object Active  extends Voice
case object Passive extends Voice

final case class VerbPhraseAnaphora(phrase: String, voice: Voice) extends VerbPhrase
