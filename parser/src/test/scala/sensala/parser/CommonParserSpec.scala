package sensala.parser

import monix.eval.Task
import sensala.SensalaSpec
import sensala.structure.noun.pronoun._
import sensala.structure.noun._

class CommonParserSpec extends SensalaSpec {
  import EnglishParserTask._

  val John = ExistentialQuantifier[Task](ProperNoun("John", Some(Person), Some(Male)))
  val Bob  = ExistentialQuantifier[Task](ProperNoun("Bob", Some(Person), Some(Male)))
  val Mary = ExistentialQuantifier[Task](ProperNoun("Mary", Some(Person), Some(Female)))
  val Ann  = ExistentialQuantifier[Task](ProperNoun("Ann", Some(Person), Some(Female)))

  val I         = FirstPersonSingularPersonalPronoun[Task]("I")
  val You       = SecondPersonSingularPersonalPronoun[Task]("You")
  val He        = ThirdPersonSingularPersonalPronoun[Task]("He", Masculine)
  val She       = ThirdPersonSingularPersonalPronoun[Task]("She", Feminine)
  val It        = ThirdPersonSingularPersonalPronoun[Task]("It", Neuter)
  val i         = FirstPersonSingularPersonalPronoun[Task]("i")
  val you       = SecondPersonSingularPersonalPronoun[Task]("you")
  val he        = ThirdPersonSingularPersonalPronoun[Task]("he", Masculine)
  val she       = ThirdPersonSingularPersonalPronoun[Task]("she", Feminine)
  val itPronoun = ThirdPersonSingularPersonalPronoun[Task]("it", Neuter)

  val him = ThirdPersonSingularPersonalPronoun[Task]("him", Masculine)

  val his = ThirdPersonSingularPossessivePronoun[Task]("his", Masculine)

  val herself = ThirdPersonSingularReflexivePronoun[Task]("herself", Feminine)
}
