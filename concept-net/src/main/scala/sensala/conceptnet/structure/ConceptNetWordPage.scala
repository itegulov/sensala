package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.ConceptNetId

final case class ConceptNetWordPage(
  id: ConceptNetId,
  context: List[String],
  edges: List[ConceptNetEdge],
  view: ConceptNetView
) extends ConceptNetStructure
