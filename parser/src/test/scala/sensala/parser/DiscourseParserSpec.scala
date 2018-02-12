package sensala.parser

import sensala.SensalaSpec
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.noun._
import sensala.structure.propositional.In
import sensala.structure.verb._
import sensala.structure.wh._

class DiscourseParserSpec extends SensalaSpec {
  it should "parse simple sentences" in {
    DiscourseParser.parse("John walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("John"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Mary loves herself").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("Mary"), TransitiveVerb("loves", ReflexivePronoun("herself")))
    ))
  }

  it should "parse quantified common nouns" in {
    DiscourseParser.parse("A donkey walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(CommonNoun("donkey"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer walks").right.value shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), IntransitiveVerb("walks"))
    ))
    DiscourseParser.parse("Every farmer owns a donkey").right.value shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))))
    ))
  }

  it should "parse wh noun phrases" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it").right.value shouldBe Discourse(List(
      ForallQuantifierVP(
        WhNounPhrase(TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        TransitiveVerb("beats", ReflexivePronoun("it"))
      )
    ))
    DiscourseParser.parse("A farmer who owns a donkey beats it").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        TransitiveVerb("beats", ReflexivePronoun("it"))
      )
    ))
    DiscourseParser.parse("A farmer who eats walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(IntransitiveVerb("eats"), CommonNoun("farmer")),
        IntransitiveVerb("walks")
      )
    ))
  }

  it should "parse multi-sentence discourses" in {
    DiscourseParser.parse("Every farmer who owns a donkey beats it. John is a farmer. John owns a donkey.").right.value shouldBe Discourse(List(
      ForallQuantifierVP(
        WhNounPhrase(TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        TransitiveVerb("beats", ReflexivePronoun("it"))
      ),
      ExistentialQuantifierVP(
        ProperNoun("John"),
        TransitiveVerb("is", ExistentialQuantifier(CommonNoun("farmer")))
      ),
      ExistentialQuantifierVP(
        ProperNoun("John"),
        TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))
  }
  
  it should "parse sentences with adjectives" in {
    DiscourseParser.parse("Every wealthy farmer owns a donkey").right.value shouldBe Discourse(List(
      ForallQuantifierVP(
        AdjectiveNounPhrase(Adjective("wealthy"), CommonNoun("farmer")),
        TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))

    DiscourseParser.parse("Every lawyer believes he is smart").right.value shouldBe Discourse(List(
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
  
  it should "parse adverb sentences" in {
    DiscourseParser.parse("John runs quickly").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("John"), VerbAdverbPhrase("quickly", IntransitiveVerb("runs")))
    ))
  }
  
  it should "parse propositional sentences" in {
    DiscourseParser.parse("John left a wallet on a table").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(
        ProperNoun("John"),
        VerbInPhrase(
          In("on", ExistentialQuantifier(CommonNoun("table"))), 
          TransitiveVerb("left", ExistentialQuantifier(CommonNoun("wallet")))
        )
      )
    ))
  }
}
