package sensala.structure.noun

import sensala.property.Property
import sensala.structure._

private[noun] object PronounPropertyMap extends {
  val reflexivePronouns = Map(
    "i"       -> Property(x => speaker(x)),
    "you"     -> Property(x => interlocutor(x)),
    "it"      -> Property(x => animal(x)),
    "he"      -> Property(x => person(x)),
    "she"     -> Property(x => female(x)),
    "him"     -> Property(x => person(x)),
    "her"     -> Property(x => female(x)),
    "itself"  -> Property(x => animal(x)),
    "himself" -> Property(x => person(x)),
    "herself" -> Property(x => female(x)),
  )
  
  val possessivePronouns = Map(
    "its" -> Property(x => animal(x)),
    "his" -> Property(x => person(x)),
    "her" -> Property(x => female(x))
  )
}
