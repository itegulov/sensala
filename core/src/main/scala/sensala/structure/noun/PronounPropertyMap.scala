package sensala.structure.noun

import sensala.structure._

private[noun] object PronounPropertyMap extends {
  val reflexivePronouns = Map(
    "it"      -> animal,
    "he"      -> person,
    "she"     -> female,
    "him"     -> person,
    "her"     -> female,
    "itself"  -> animal,
    "himself" -> person,
    "herself" -> female,
  )
  
  val possessivePronouns = Map(
    "its" -> animal,
    "his" -> person,
    "her" -> female
  )
}
