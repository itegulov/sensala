package sensala.parser

import edu.stanford.nlp.trees.UniversalEnglishGrammaticalRelations

object SensalaGrammaticalRelations {
  val Det = UniversalEnglishGrammaticalRelations.DETERMINER
  val AdjMod = UniversalEnglishGrammaticalRelations.ADJECTIVAL_MODIFIER
  val AdvMod = UniversalEnglishGrammaticalRelations.ADVERBIAL_MODIFIER
  val Ref = UniversalEnglishGrammaticalRelations.REFERENT
  val RelClMod = UniversalEnglishGrammaticalRelations.RELATIVE_CLAUSE_MODIFIER
  val NomMod = UniversalEnglishGrammaticalRelations.NOMINAL_MODIFIER
  val DObj = UniversalEnglishGrammaticalRelations.DIRECT_OBJECT
  val NSubj = UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT
  val CComp = UniversalEnglishGrammaticalRelations.CLAUSAL_COMPLEMENT
  val Cop = UniversalEnglishGrammaticalRelations.COPULA
  val Case = UniversalEnglishGrammaticalRelations.CASE_MARKER
}
