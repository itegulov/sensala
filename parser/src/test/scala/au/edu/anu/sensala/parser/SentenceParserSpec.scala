package au.edu.anu.sensala.parser

import au.edu.anu.sensala.SensalaSpec
import au.edu.anu.sensala.structure._

class SentenceParserSpec extends SensalaSpec {
  it should "parse simple sentences" in {
    SentenceParser.parse("John walks") shouldBe Sentence(ProperNoun("John"), IntransitiveVerb("walks"))
    SentenceParser.parse("Mary loves herself") shouldBe Sentence(ProperNoun("Mary"), VerbObjPhrase(TransitiveVerb("loves"), ReflexivePronoun("herself")))
  }

  it should "parse quantified common nouns" in {
    SentenceParser.parse("A donkey walks") shouldBe Sentence(ExistentialQuantifier(CommonNoun("donkey")), IntransitiveVerb("walks"))
    SentenceParser.parse("Every farmer walks") shouldBe Sentence(ForallQuantifier(CommonNoun("farmer")), IntransitiveVerb("walks"))
    SentenceParser.parse("Every farmer owns a donkey") shouldBe Sentence(ForallQuantifier(CommonNoun("farmer")), VerbObjPhrase(TransitiveVerb("owns"), ExistentialQuantifier(CommonNoun("donkey"))))
  }
}
