package sensala.parser.english

import sensala.models.nl._
import sensala.parser.ParserSpec
import sensala.shared.effect.scalatestcontrib._

class EnglishDiscourseParserSpec extends ParserSpec {
  "English discourse parser" should "parse simple sentences" in effectTest {
    for {
      _ <- parser.parse("John walks").shouldBeF(sentence(John, IntransitiveVerb("walks")))
      _ <- parser
            .parse("Mary loves herself")
            .shouldBeF(
              sentence(
                Mary,
                TransitiveVerb("loves", herself)
              )
            )
    } yield ()
  }

  it should "parse quantified common nouns" in effectTest {
    for {
      _ <- parser
            .parse("A donkey walks")
            .shouldBeF(
              sentence(
                ExistentialQuantifier(CommonNoun("donkey")),
                IntransitiveVerb("walks")
              )
            )
      _ <- parser
            .parse("Every farmer walks")
            .shouldBeF(
              sentence(
                ForallQuantifier(CommonNoun("farmer")),
                IntransitiveVerb("walks")
              )
            )
      _ <- parser
            .parse("Every farmer owns a donkey")
            .shouldBeF(
              sentence(
                ForallQuantifier(CommonNoun("farmer")),
                TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
              )
            )
    } yield ()
  }

  it should "parse wh noun phrases" in effectTest {
    for {
      _ <- parser
            .parse("Every farmer who owns a donkey beats it")
            .shouldBeF(
              sentence(
                ForallQuantifier(
                  WhNounPhrase(
                    TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
                    CommonNoun("farmer")
                  )
                ),
                TransitiveVerb("beats", itPronoun)
              )
            )
      _ <- parser
            .parse("A farmer who owns a donkey beats it")
            .shouldBeF(
              sentence(
                ExistentialQuantifier(
                  WhNounPhrase(
                    TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
                    CommonNoun("farmer")
                  )
                ),
                TransitiveVerb("beats", itPronoun)
              )
            )
      _ <- parser
            .parse("A farmer who eats walks")
            .shouldBeF(
              sentence(
                ExistentialQuantifier(WhNounPhrase(IntransitiveVerb("eats"), CommonNoun("farmer"))),
                IntransitiveVerb("walks")
              )
            )
    } yield ()
  }

  it should "parse multi-sentence discourses" in effectTest {
    parser
      .parse("Every farmer who owns a donkey beats it. John is a farmer. John owns a donkey.")
      .shouldBeF(
        Discourse(
          List(
            Sentence(
              ForallQuantifier(
                WhNounPhrase(
                  TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
                  CommonNoun("farmer")
                )
              ),
              TransitiveVerb("beats", itPronoun)
            ),
            Sentence(
              John,
              TransitiveVerb("is", ExistentialQuantifier(CommonNoun("farmer")))
            ),
            Sentence(
              John,
              TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
            )
          )
        )
      )
  }

  it should "parse sentences with adjectives" in effectTest {
    for {
      _ <- parser
            .parse("Every wealthy farmer owns a donkey")
            .shouldBeF(
              sentence(
                ForallQuantifier(AdjectiveNounPhrase(Adjective("wealthy"), CommonNoun("farmer"))),
                TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
              )
            )

      _ <- parser
            .parse("Every lawyer believes he is smart")
            .shouldBeF(
              sentence(
                ForallQuantifier(CommonNoun("lawyer")),
                VerbSentencePhrase(
                  "believes",
                  Sentence(he, VerbAdjectivePhrase("is", Adjective("smart")))
                )
              )
            )
    } yield ()
  }

  it should "parse adverb sentences" in effectTest {
    parser
      .parse("John runs quickly")
      .shouldBeF(
        sentence(
          John,
          VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs"))
        )
      )
  }

  it should "parse propositional sentences" in effectTest {
    parser
      .parse("John left a wallet on a table")
      .shouldBeF(
        sentence(
          John,
          VerbInPhrase(
            InPhrase("on", ExistentialQuantifier(CommonNoun("table"))),
            TransitiveVerb("left", ExistentialQuantifier(CommonNoun("wallet")))
          )
        )
      )
  }

  it should "parse comparatives" in effectTest {
    parser
      .parse("John is smarter than Bob")
      .shouldBeF(
        sentence(
          John,
          VerbComparativePhrase("smarter", Bob)
        )
      )
  }

  it should "parse relative clauses" in effectTest {
    parser
      .parse("John loves Mary that loves him")
      .shouldBeF(
        sentence(
          John,
          TransitiveVerb(
            "loves",
            ExistentialQuantifier(
              RelativeClausePhrase(
                "that",
                TransitiveVerb("loves", him),
                ProperNoun("Mary", Some(Person), Some(Female))
              )
            )
          )
        )
      )
  }
}
