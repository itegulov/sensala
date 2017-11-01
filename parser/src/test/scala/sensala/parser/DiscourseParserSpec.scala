package sensala.parser

import sensala.SensalaSpec
import sensala.structure._

class DiscourseParserSpec extends SensalaSpec {
  it should "parse simple sentences" in {
    DiscourseParser.parse("John walks") shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("John"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Mary loves herself") shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("Mary"), VerbObjPhrase("loves", ReflexivePronoun("herself")))
    ))
  }

  it should "parse quantified common nouns" in {
    DiscourseParser.parse("A donkey walks") shouldBe Discourse(List(
      ExistentialQuantifierVP(CommonNoun("donkey"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer walks") shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer owns a donkey") shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey"))))
    ))
  }

  it should "parse wh noun phrases" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it") shouldBe Discourse(List(
      ForallQuantifierVP(
        WhNounPhrase(VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        VerbObjPhrase("beats", ReflexivePronoun("it"))
      )
    ))
    DiscourseParser.parse("A farmer who owns a donkey beats it") shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        VerbObjPhrase("beats", ReflexivePronoun("it"))
      )
    ))
    DiscourseParser.parse("A farmer who eats walks") shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(IntransitiveVerb("eats"), CommonNoun("farmer")),
        IntransitiveVerb("walks")
      )
    ))
  }

  it should "parse multi-sentence discourses" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it. John is a farmer. John owns a donkey.") shouldBe Discourse(List(
      ForallQuantifierVP(
        WhNounPhrase(VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        VerbObjPhrase("beats", ReflexivePronoun("it"))
      ),
      ExistentialQuantifierVP(
        ProperNoun("John"),
        VerbObjPhrase("is", ExistentialQuantifier(CommonNoun("farmer")))
      ),
      ExistentialQuantifierVP(
        ProperNoun("John"),
        VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))
  }
  
  it should "parse sentences with adjectives" in {
    DiscourseParser.parse("Every wealthy farmer owns a donkey") shouldBe Discourse(List(
      ForallQuantifierVP(
        AdjectivePhrase(Adjective("wealthy"), CommonNoun("farmer")),
        VerbObjPhrase("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))

    DiscourseParser.parse("Every lawyer believes he is smart") shouldBe Discourse(List(
      
      ForallQuantifierVP(
        CommonNoun("lawyer"),
        VerbSentencePhrase(
          "believes",
          ReflexivePronounVP(
            "he",
            VerbAdjectivePhrase("is", Adjective("smart"))
          )
        )
      )
    ))
  }
}
