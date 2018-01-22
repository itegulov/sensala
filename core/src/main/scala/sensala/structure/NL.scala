package sensala.structure

trait NL {
  def interpret(cont: NLEffE): NLEffE
}

trait Word extends NL {
  val word: String
}
