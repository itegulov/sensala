package au.edu.anu.sensala.structure

trait NL {
  def interpret(e: Context): L
}

trait Word extends NL {
  val word: String
  def interpret(e: Context) = Sym(word)
}