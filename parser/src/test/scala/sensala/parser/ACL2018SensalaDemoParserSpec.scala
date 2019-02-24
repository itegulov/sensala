package sensala.parser

import monix.eval.Task
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.adverb._
import sensala.structure.noun._
import sensala.structure.noun.pronoun._
import sensala.structure.prepositional._
import sensala.structure.verb._
import sensala.structure.wh._

class ACL2018SensalaDemoParserSpec extends CommonParserSpec {
  import EnglishParserTask._

  it should "parse examples from ACL 2018 system demonstration paper" in {
    parse("John loves Mary").right.value shouldBe Discourse[Task](
      List(
        Sentence(John, TransitiveVerb("loves", Mary))
      )
    )

    parse("John runs quickly").right.value shouldBe Discourse[Task](
      List(
        Sentence(John, VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs")))
      )
    )

    parse("John loves Mary. She believes him.").right.value shouldBe Discourse[Task](
      List(
        Sentence(John, TransitiveVerb("loves", Mary)),
        Sentence(She, TransitiveVerb("believes", him))
      )
    )

    parse("John loves Mary. She believes this.").right.value shouldBe Discourse[Task](
      List(
        Sentence(John, TransitiveVerb("loves", Mary)),
        Sentence(She, TransitiveVerb("believes", DemonstrativePronoun("this")))
      )
    )

    parse("John ate a pizza with a fork. Bob did too.").right.value shouldBe Discourse[Task](
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

    parse("Mary is loved by John. So is Ann.").right.value shouldBe Discourse[Task](
      List(
        Sentence(John, TransitiveVerb("loved", Mary)),
        Sentence(
          Ann,
          VerbPhraseAnaphora("So is", Passive)
        )
      )
    )

    parse("Every farmer who owns a donkey thinks he is rich").right.value shouldBe Discourse[Task](
      List(
        Sentence(
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
    )

    parse("John bought a donkey. The animal was stubborn as hell.").right.value shouldBe
      Discourse[Task](
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

    parse("John left his wallet on a table").right.value shouldBe Discourse[Task](
      List(
        Sentence(
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
    )
  }
}
