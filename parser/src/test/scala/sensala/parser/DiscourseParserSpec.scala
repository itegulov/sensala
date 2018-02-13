package sensala.parser

import sensala.SensalaSpec
import sensala.property.{CachedPropertyExtractor, ConceptNetPropertyExtractor}
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.noun._
import sensala.structure.verb._
import sensala.structure.wh._

class DiscourseParserSpec extends SensalaSpec {
  implicit val propertyExtractor = CachedPropertyExtractor(ConceptNetPropertyExtractor)
  val discourseParser = DiscourseParser()
  
  it should "parse simple sentences" in {
    discourseParser.parse("John walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("John"), IntransitiveVerb("walks"))
    ))
    discourseParser.parse("Mary loves herself").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("Mary"), TransitiveVerb("loves", ReflexivePronoun("herself")))
    ))
  }

  it should "parse quantified common nouns" in {
    discourseParser.parse("A donkey walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(CommonNoun("donkey"), IntransitiveVerb("walks"))
    ))
    discourseParser.parse("Every farmer walks").right.value shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), IntransitiveVerb("walks"))
    ))
    discourseParser.parse("Every farmer owns a donkey").right.value shouldBe Discourse(List(
      ForallQuantifierVP(CommonNoun("farmer"), TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))))
    ))
  }

  it should "parse wh noun phrases" in {
    discourseParser.parse("Every farmer who owns a donkey beats it").right.value shouldBe Discourse(List(
      ForallQuantifierVP(
        WhNounPhrase(TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        TransitiveVerb("beats", ReflexivePronoun("it"))
      )
    ))
    discourseParser.parse("A farmer who owns a donkey beats it").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))), CommonNoun("farmer")),
        TransitiveVerb("beats", ReflexivePronoun("it"))
      )
    ))
    discourseParser.parse("A farmer who eats walks").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(
        WhNounPhrase(IntransitiveVerb("eats"), CommonNoun("farmer")),
        IntransitiveVerb("walks")
      )
    ))
  }

  it should "parse multi-sentence discourses" in {
    discourseParser.parse("Every farmer who owns a donkey beats it. John is a farmer. John owns a donkey.").right.value shouldBe Discourse(List(
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
    discourseParser.parse("Every wealthy farmer owns a donkey").right.value shouldBe Discourse(List(
      ForallQuantifierVP(
        AdjectiveNounPhrase(Adjective("wealthy"), CommonNoun("farmer")),
        TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ))

    discourseParser.parse("Every lawyer believes he is smart").right.value shouldBe Discourse(List(
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
    discourseParser.parse("John runs quickly").right.value shouldBe Discourse(List(
      ExistentialQuantifierVP(ProperNoun("John"), VerbAdverbPhrase("quickly", IntransitiveVerb("runs")))
    ))
  }
}
