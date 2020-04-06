package sensala.parser

import sensala.models.nl._
import sensala.shared.effect.scalatestcontrib._

class ACL2018SensalaDemoParserSpec extends ParserSpec {
  "English discourse parser" should "parse examples from ACL 2018 system demonstration paper" in effectTest {
    for {
      _ <- parser.parse("John loves Mary").shouldBeF(sentence(John, TransitiveVerb("loves", Mary)))

      _ <- parser
            .parse("John runs quickly")
            .shouldBeF(
              sentence(John, VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs")))
            )

      _ <- parser
            .parse("John loves Mary. She believes him.")
            .shouldBeF(
              Discourse(
                List(
                  Sentence(John, TransitiveVerb("loves", Mary)),
                  Sentence(She, TransitiveVerb("believes", him))
                )
              )
            )

      _ <- parser
            .parse("John loves Mary. She believes this.")
            .shouldBeF(
              Discourse(
                List(
                  Sentence(John, TransitiveVerb("loves", Mary)),
                  Sentence(She, TransitiveVerb("believes", DemonstrativePronoun("this")))
                )
              )
            )

      _ <- parser
            .parse("John ate a pizza with a fork. Bob did too.")
            .shouldBeF(
              Discourse(
                List(
                  Sentence(
                    John,
                    VerbInPhrase(
                      InPhrase("with", ExistentialQuantifier(CommonNoun("fork"))),
                      TransitiveVerb("ate", ExistentialQuantifier(CommonNoun("pizza")))
                    )
                  ),
                  Sentence(
                    Bob,
                    VerbPhraseAnaphora("did too", Active)
                  )
                )
              )
            )

      _ <- parser
            .parse("Mary is loved by John. So is Ann.")
            .shouldBeF(
              Discourse(
                List(
                  Sentence(John, TransitiveVerb("loved", Mary)),
                  Sentence(
                    Ann,
                    VerbPhraseAnaphora("So is", Passive)
                  )
                )
              )
            )

      _ <- parser
            .parse("Every farmer who owns a donkey thinks he is rich")
            .shouldBeF(
              sentence(
                ForallQuantifier(
                  WhNounPhrase(
                    TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
                    CommonNoun("farmer")
                  )
                ),
                VerbSentencePhrase(
                  "thinks",
                  Sentence(he, VerbAdjectivePhrase("is", Adjective("rich")))
                )
              )
            )

      _ <- parser
            .parse("John bought a donkey. The animal was stubborn as hell.")
            .shouldBeF(
              Discourse(
                List(
                  Sentence(
                    John,
                    TransitiveVerb("bought", ExistentialQuantifier(CommonNoun("donkey")))
                  ),
                  Sentence(
                    DefiniteNounPhrase(CommonNoun("animal")),
                    VerbInPhrase(
                      InPhrase("as", ExistentialQuantifier(CommonNoun("hell"))),
                      VerbAdjectivePhrase("was", Adjective("stubborn"))
                    )
                  )
                )
              )
            )

      _ <- parser
            .parse("John left his wallet on a table")
            .shouldBeF(
              sentence(
                John,
                VerbInPhrase(
                  InPhrase("on", ExistentialQuantifier(CommonNoun("table"))),
                  TransitiveVerb(
                    "left",
                    ExistentialQuantifier(
                      NounPhrasePreposition(
                        PossessionPhrase(his),
                        CommonNoun("wallet")
                      )
                    )
                  )
                )
              )
            )
    } yield ()
  }
}
