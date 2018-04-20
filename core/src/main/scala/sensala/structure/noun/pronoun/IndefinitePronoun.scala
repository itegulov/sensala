package sensala.structure.noun.pronoun

import org.aossie.scavenger.expression.E
import org.aossie.scavenger.expression.formula._
import org.atnos.eff.all._
import sensala.property.Property
import sensala.structure._
import sensala.structure.types._

sealed trait IndefinitePronoun extends Pronoun

sealed trait SingularIndefinitePronoun extends IndefinitePronoun

sealed trait PersonSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List(Property(x => person(x)))

  override def definiteProperties: List[Property] = properties
}
final case class NegativePersonSingularIndefinitePronoun(word: String) extends PersonSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield ~Ex(x, entity, contL)
}
final case class UniversalPersonSingularIndefinitePronoun(word: String) extends PersonSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield All(x, entity, contL)
}
final case class ExistentialPersonSingularIndefinitePronoun(word: String) extends PersonSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield Ex(x, entity, contL)
}

sealed trait ThingSingularIndefinitePronoun extends SingularIndefinitePronoun {
  override def properties: List[Property] = List()

  override def definiteProperties: List[Property] = properties
}
final case class NegativeThingSingularIndefinitePronoun(word: String) extends ThingSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield ~Ex(x, entity, contL)
}
final case class UniversalThingSingularIndefinitePronoun(word: String) extends ThingSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield All(x, entity, contL)
}
final case class ExistentialThingSingularIndefinitePronoun(word: String) extends ThingSingularIndefinitePronoun {
  override def interpret(cont: NLEff[E]): NLEff[E] =
    for {
      x <- bindFreeVar
      _ <- modify[NLFx, Context](_.addEntity(x, properties))
      _ <- putEntity(x)
      contL <- cont
    } yield Ex(x, entity, contL)
}