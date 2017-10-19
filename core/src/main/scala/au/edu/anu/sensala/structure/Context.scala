package au.edu.anu.sensala.structure

// TODO: make context more general, generic and abstract
case class Context(referents: List[Sym]) {
  def findAnaphoricReferent: Option[Sym] = referents.headOption
  def extend(newRef: Sym): Context = Context(newRef :: referents)
}
