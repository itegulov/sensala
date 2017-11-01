package sensala.structure

trait NL {
  def interpret: CState
}

trait NounPhrase extends NL {
  def gender: Gender
}

final case class Adjective(word: String) extends Word {
  override def interpret = ???
}

trait Word extends NL {
  val word: String
}
