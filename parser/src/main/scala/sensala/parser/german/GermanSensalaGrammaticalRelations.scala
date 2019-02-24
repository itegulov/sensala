package sensala.parser.german

import edu.stanford.nlp.international.Language
import edu.stanford.nlp.trees.{GrammaticalRelation, UniversalEnglishGrammaticalRelations}

object GermanSensalaGrammaticalRelations {
  val Det        = GrammaticalRelation.valueOf(Language.German, "det")
  val AdjMod     = GrammaticalRelation.valueOf(Language.German, "amod")
  val AdvMod     = GrammaticalRelation.valueOf(Language.German, "advmod")
  val ClMod      = GrammaticalRelation.valueOf(Language.German, "acl")
  val NomMod     = GrammaticalRelation.valueOf(Language.German, "nmod")
  val NomModOn   = UniversalEnglishGrammaticalRelations.valueOf("nmod:on")
  val NomModPoss = UniversalEnglishGrammaticalRelations.valueOf("nmod:poss")
  val DObj       = GrammaticalRelation.valueOf(Language.German, "dobj")
  val NSubj      = GrammaticalRelation.valueOf(Language.German, "nsubj")
  val NSubjPass  = UniversalEnglishGrammaticalRelations.NOMINAL_PASSIVE_SUBJECT
  val CComp      = UniversalEnglishGrammaticalRelations.CLAUSAL_COMPLEMENT
  val Cop        = UniversalEnglishGrammaticalRelations.COPULA
  val Case       = UniversalEnglishGrammaticalRelations.CASE_MARKER
  val AuxPass    = UniversalEnglishGrammaticalRelations.AUX_PASSIVE_MODIFIER
}
