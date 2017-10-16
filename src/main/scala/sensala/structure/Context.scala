package sensala.structure

// TODO: make context more general, generic and abstract
class Context(val referents: List[Sym]) {
  def findAnaphoricReferent() = referents.head // FIXME : This throws exception when list is empty
}
