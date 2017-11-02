package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.ConceptNetId

final case class ConceptNetSource(
  id: ConceptNetId,
  contributor: String,
  process: Option[String]
) extends ConceptNetStructure
