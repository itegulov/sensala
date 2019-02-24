package sensala.parser

import sensala.structure.noun._

trait DiscourseParser[F[_]] {
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
