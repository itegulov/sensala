package au.edu.anu.sensala.structure

trait NounPhrase extends NL {
  def interpret(e: Context): Sym
}

case class ProperNoun(word: String) extends Word with NounPhrase
case class CommonNoun(word: String) extends Word with NounPhrase

case class ReflexivePronoun(word: String) extends Word with NounPhrase {
  override def interpret(e: Context) = e.findAnaphoricReferent()
}
