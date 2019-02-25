package sensala.structure

sealed trait NamedEntityType

case object Location     extends NamedEntityType
case object Person       extends NamedEntityType
case object Organization extends NamedEntityType

sealed trait Misc   extends NamedEntityType
case object Money   extends Misc
case object Percent extends Misc
case object Date    extends Misc
case object Time    extends Misc
