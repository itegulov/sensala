package sensala.structure.noun

import sensala.structure._

private[noun] object PronounPropertyMap extends {
  val map = Map(
    "it"      -> animal,
    "he"      -> male,
    "she"     -> female,
    "him"     -> male,
    "her"     -> female,
    "itself"  -> animal,
    "himself" -> male,
    "herself" -> female,
  )
}
