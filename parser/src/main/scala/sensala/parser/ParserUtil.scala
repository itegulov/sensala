package sensala.parser

import sensala.models.nl._

object ParserUtil {
  def pairToTuple[U, V](p: edu.stanford.nlp.util.Pair[U, V]): (U, V) =
    (p.first, p.second)

  def parseNer(nerString: String): Option[NamedEntityType] = nerString match {
    case "LOCATION"     => Some(Person)
    case "PERSON"       => Some(Person)
    case "ORGANIZATION" => Some(Organization)
    case "MONEY"        => Some(Money)
    case "PERCENT"      => Some(Percent)
    case "DATE"         => Some(Date)
    case "TIME"         => Some(Time)
    case _              => None
  }

  def parseGender(genderString: String): Option[NamedEntityGender] = genderString match {
    case "MALE"   => Some(Male)
    case "FEMALE" => Some(Female)
    case _        => None
  }
}
