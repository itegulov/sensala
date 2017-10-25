package sensala.parser

import sensala.SensalaSpec
import sensala.structure._

class DiscourseParserSpec extends SensalaSpec {
  it should "parse simple sentences" in {
    DiscourseParser.parse("John walks") shouldBe Discourse(List(
      Sentence(ProperNoun("John"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Mary loves herself") shouldBe Discourse(List(
      Sentence(ProperNoun("Mary"), VerbObjPhrase(TransitiveVerb("loves"), ReflexivePronoun("herself")))
    ))
  }

  it should "parse quantified common nouns" in {
    DiscourseParser.parse("A donkey walks") shouldBe Discourse(List(
      Sentence(ExistentialQuantifier(CommonNoun("donkey")), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer walks") shouldBe Discourse(List(
      Sentence(ForallQuantifier(CommonNoun("farmer")), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer owns a donkey") shouldBe Discourse(List(
      Sentence(ForallQuantifier(CommonNoun("farmer")), VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey"))))
    ))
  }

  it should "parse wh noun phrases" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it") shouldBe Discourse(List(
      Sentence(
        ForallQuantifier(WhNounPhrase(VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer"))),
        VerbObjPhrase(TransitiveVerb("beats"), ReflexivePronoun("it"))
      )
    ))
    DiscourseParser.parse("A farmer who eats walks") shouldBe Discourse(List(
      Sentence(
        ExistentialQuantifier(WhNounPhrase(IntransitiveVerb("eats"), CommonNoun("farmer"))),
        IntransitiveVerb("walks")
      )
    ))
  }

  it should "parse multi-sentence discourses" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it. John is a farmer. John owns a donkey.") shouldBe Discourse(List(
      Sentence(
        ForallQuantifier(WhNounPhrase(VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer"))),
        VerbObjPhrase(TransitiveVerb("beats"), ReflexivePronoun("it"))
      ),
      Sentence(
        ProperNoun("John"),
        VerbObjPhrase(TransitiveVerb("is"), ExistentialQuantifier(CommonNoun("farmer")))
      ),
      Sentence(
        ProperNoun("John"),
        VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))
  }
  
  it should "parse sentences with adjectives" in {
    DiscourseParser.parse("Every wealthy farmer owns a donkey") shouldBe Discourse(List(
      Sentence(
        ForallQuantifier(AdjectivePhrase(Adjective("wealthy"), CommonNoun("farmer"))),
        VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey"))))
    ))

    DiscourseParser.parse("Every lawyer believes he is smart") shouldBe Discourse(List(
      Sentence(
        ForallQuantifier(CommonNoun("lawyer")),
        VerbSentencePhrase(
          TransitiveVerb("believes"),
          Sentence(
            ReflexivePronoun("he"),
            VerbAdjectivePhrase(TransitiveVerb("is"), Adjective("smart"))
          )
        )
      )
    ))
  }
}
