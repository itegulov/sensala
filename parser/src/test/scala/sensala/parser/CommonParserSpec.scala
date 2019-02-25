package sensala.parser

import sensala.SensalaSpec
import sensala.structure.noun.pronoun._
import sensala.structure.noun._

class CommonParserSpec extends SensalaSpec {
  val John = ExistentialQuantifier(ProperNoun("John", Some(Person), Some(Male)))
  val Bob  = ExistentialQuantifier(ProperNoun("Bob", Some(Person), Some(Male)))
  val Mary = ExistentialQuantifier(ProperNoun("Mary", Some(Person), Some(Female)))
  val Ann  = ExistentialQuantifier(ProperNoun("Ann", Some(Person), Some(Female)))

  val I         = FirstPersonSingularPersonalPronoun("I")
  val You       = SecondPersonSingularPersonalPronoun("You")
  val He        = ThirdPersonSingularPersonalPronoun("He", Masculine)
  val She       = ThirdPersonSingularPersonalPronoun("She", Feminine)
  val It        = ThirdPersonSingularPersonalPronoun("It", Neuter)
  val i         = FirstPersonSingularPersonalPronoun("i")
  val you       = SecondPersonSingularPersonalPronoun("you")
  val he        = ThirdPersonSingularPersonalPronoun("he", Masculine)
  val she       = ThirdPersonSingularPersonalPronoun("she", Feminine)
  val itPronoun = ThirdPersonSingularPersonalPronoun("it", Neuter)

  val him = ThirdPersonSingularPersonalPronoun("him", Masculine)

  val his = ThirdPersonSingularPossessivePronoun("his", Masculine)

  val herself = ThirdPersonSingularReflexivePronoun("herself", Feminine)
}
