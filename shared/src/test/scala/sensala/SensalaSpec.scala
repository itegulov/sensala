package sensala

import org.scalatest._
import org.scalatest.concurrent.ScalaFutures
import sensala.models.nl._

class SensalaSpec
    extends FlatSpec
    with Matchers
    with Inspectors
    with Inside
    with OptionValues
    with EitherValues
    with TryValues
    with ScalaFutures {
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
