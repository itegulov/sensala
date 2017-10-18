package sensala.parser

import sensala.SensalaSpec
import sensala.structure._

class SentenceParserSpec extends SensalaSpec {
  it should "parse simple sentences" in {
    SentenceParser.parse("John walks") shouldBe Sentence(ProperNoun("John"), IntransitiveVerb("walks"))
    SentenceParser.parse("John walks dog") shouldBe Sentence(ProperNoun("John"), VerbObjPhrase(TransitiveVerb("walks"), CommonNoun("dog")))
    SentenceParser.parse("Mary loves herself") shouldBe Sentence(ProperNoun("Mary"), VerbObjPhrase(TransitiveVerb("loves"), ReflexivePronoun("herself")))
  }
}
