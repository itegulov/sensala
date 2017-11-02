package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.ConceptNetId

final case class ConceptNetWord(
  id: ConceptNetId,
  context: List[String],
  edges: List[ConceptNetEdge]
) extends ConceptNetStructure
