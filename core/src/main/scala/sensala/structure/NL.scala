package sensala.structure

trait NL {
  def interpret: CState
}

trait Word extends NL {
  val word: String
}
