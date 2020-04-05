package sensala.parser

import cats.instances.either._
import cats.mtl.instances.handle._
import sensala.SensalaSpec
import sensala.parser.english._

trait ParserSpec extends SensalaSpec {
  type EitherS[A] = Either[ParserError, A]

  implicit val pronounParser = new PronounParser[EitherS]
  implicit val parser        = new EnglishDiscourseParser[EitherS]
}
