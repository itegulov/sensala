package sensala.parser.english

import edu.stanford.nlp.trees.UniversalEnglishGrammaticalRelations

object EnglishSensalaGrammaticalRelations {
  val Det        = UniversalEnglishGrammaticalRelations.DETERMINER
  val AdjMod     = UniversalEnglishGrammaticalRelations.ADJECTIVAL_MODIFIER
  val AdvMod     = UniversalEnglishGrammaticalRelations.ADVERBIAL_MODIFIER
  val Ref        = UniversalEnglishGrammaticalRelations.REFERENT
  val RelClMod   = UniversalEnglishGrammaticalRelations.RELATIVE_CLAUSE_MODIFIER
  val NomMod     = UniversalEnglishGrammaticalRelations.NOMINAL_MODIFIER
  val NomModOn   = UniversalEnglishGrammaticalRelations.valueOf("nmod:on")
  val NomModPoss = UniversalEnglishGrammaticalRelations.valueOf("nmod:poss")
  val DObj       = UniversalEnglishGrammaticalRelations.DIRECT_OBJECT
  val NSubj      = UniversalEnglishGrammaticalRelations.NOMINAL_SUBJECT
  val NSubjPass  = UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT
  val CComp      = UniversalEnglishGrammaticalRelations.CLAUSAL_COMPLEMENT
  val Cop        = UniversalEnglishGrammaticalRelations.COPULA
  val Case       = UniversalEnglishGrammaticalRelations.CASE_MARKER
  val AuxPass    = UniversalEnglishGrammaticalRelations.AUX_PASSIVE_MODIFIER
}
