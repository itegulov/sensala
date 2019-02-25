package sensala.structure.noun.pronoun

import sensala.property.Property
import sensala.structure._

sealed trait IndefinitePronoun extends Pronoun

sealed trait SingularIndefinitePronoun extends IndefinitePronoun

sealed trait PersonSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List(Property(x => person(x)))

  override def definiteProperties: List[Property] = properties
}
final case class NegativePersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class UniversalPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun
final case class ExistentialPersonSingularIndefinitePronoun(
  word: String
) extends PersonSingularIndefinitePronoun

sealed trait ThingSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
final case class NegativeThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun
final case class UniversalThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun
final case class ExistentialThingSingularIndefinitePronoun(
  word: String
) extends ThingSingularIndefinitePronoun
