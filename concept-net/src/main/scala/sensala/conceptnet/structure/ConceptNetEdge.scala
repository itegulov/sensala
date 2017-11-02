package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.{ConceptNetId, ConceptNetRelation}

final case class ConceptNetEdge(
  id: ConceptNetId,
  start: ConceptNetEndpoint,
  end: ConceptNetEndpoint,
  dataset: String,
  license: String,
  relation: ConceptNetRelation,
  sources: List[ConceptNetSource],
  surfaceText: Option[String],
  weight: Double
) extends ConceptNetStructure
