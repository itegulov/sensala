package sensala.models.nl

sealed trait NamedEntityGender
case object Male   extends NamedEntityGender
case object Female extends NamedEntityGender
