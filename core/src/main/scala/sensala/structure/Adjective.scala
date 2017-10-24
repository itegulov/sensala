package sensala.structure

final case class Adjective(word: String) extends Word {
  override def interpret: CState = ???
}

final case class AdjectivePhrase(adjective: Adjective, nounPhrase: NounPhrase) extends NounPhrase {
  override def interpret: CState = ???
}
