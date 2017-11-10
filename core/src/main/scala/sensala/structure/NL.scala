package sensala.structure

trait NL {
  def interpret(cont: CState): CState
}

trait Word extends NL {
  val word: String
}
