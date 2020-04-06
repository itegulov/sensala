package sensala.parser

import cats.Monad
import cats.effect.IO
import distage.Injector
import izumi.distage.model.definition.ModuleDef
import izumi.distage.model.plan.GCMode
import sensala.SensalaSpec
import sensala.parser.english.ParserError.HandleParserError
import sensala.parser.english.ParserError.HandleParserError.handleParserErrorIO
import sensala.parser.english._

trait ParserSpec extends SensalaSpec {
  val testModule = new ModuleDef {
    make[PronounParser[IO]]
    make[NounPhraseParser[IO]]
    make[VerbPhraseParser[IO]]
    make[EnglishDiscourseParser[IO]]
    addImplicit[Monad[IO]]
    addImplicit[HandleParserError[IO]]
  }

  val plan     = Injector().plan(testModule, GCMode.NoGC)
  val resource = Injector().produce(plan)
  val parser = resource.use { objects =>
    objects.get[EnglishDiscourseParser[IO]]
  }
}
