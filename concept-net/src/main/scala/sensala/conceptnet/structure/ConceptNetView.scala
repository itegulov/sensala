package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.ConceptNetId

final case class ConceptNetView(
  id: ConceptNetId,
  firstPage: String,
  nextPage: Option[String],
  previousPage: Option[String],
  paginatedProperty: String
) extends ConceptNetStructure
