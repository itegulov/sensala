package sensala.conceptnet.structure

import sensala.conceptnet.structure.auxilary.{ConceptNetId, ConceptNetLanguage}

final case class ConceptNetEndpoint(
  id: ConceptNetId,
  label: String,
  language: ConceptNetLanguage,
  term: String
) extends ConceptNetStructure
