package sensala.parser

import sensala.SensalaSpec
import sensala.structure._
import sensala.structure.adjective._
import sensala.structure.adverb._
import sensala.structure.noun._
import sensala.structure.prepositional._
import sensala.structure.verb._
import sensala.structure.wh._

class ACL2018SensalaDemoSpec extends SensalaSpec {
  def parse(discourse: String): Either[String, Discourse] = {
    DiscourseParser.parse(discourse)
  }
  
  it should "parse example from ACL 2018 system demonstration paper" in {
    parse("John loves Mary").right.value shouldBe Discourse(List(
      Sentence(ExistentialQuantifier(ProperNoun("John")), TransitiveVerb("loves", ExistentialQuantifier(ProperNoun("Mary"))))
    ))
    
    parse("John runs quickly").right.value shouldBe Discourse(List(
      Sentence(ExistentialQuantifier(ProperNoun("John")), VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs")))
    ))
    
    parse("John loves Mary. She believes him.").right.value shouldBe Discourse(List(
      Sentence(ExistentialQuantifier(ProperNoun("John")), TransitiveVerb("loves", ExistentialQuantifier(ProperNoun("Mary")))),
      Sentence(ReflexivePronoun("She"), TransitiveVerb("believes", ReflexivePronoun("him")))
    ))

    parse("John ate a pizza with a fork. Bob did too.").right.value shouldBe Discourse(List(
      Sentence(
        ExistentialQuantifier(ProperNoun("John")),
        VerbInPhrase(
          InPhrase("with", ExistentialQuantifier(CommonNoun("fork"))),
          TransitiveVerb("ate", ExistentialQuantifier(CommonNoun("pizza")))
        )
      ),
      Sentence(
        ExistentialQuantifier(ProperNoun("Bob")),
        VerbPhraseAnaphora("did too")
      )
    ))

    parse("Mary is loved by John. So is Ann.").right.value shouldBe Discourse(List(
      Sentence(ExistentialQuantifier(ProperNoun("John")), TransitiveVerb("loved", ExistentialQuantifier(ProperNoun("Mary")))),
      Sentence(
        ExistentialQuantifier(ProperNoun("Ann")),
        VerbPhraseAnaphora("So is")
      )
    ))

    parse("Every farmer who owns a donkey thinks he is rich").right.value shouldBe Discourse(List(
      Sentence(
        WhNounPhrase(
          TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
          ForallQuantifier(CommonNoun("farmer"))
        ),
        VerbSentencePhrase("thinks", Sentence(ReflexivePronoun("he"), VerbAdjectivePhrase("is", Adjective("rich"))))
      )
    ))
    
    parse("John bought a donkey. The animal was stubborn as hell.").right.value shouldBe Discourse(List(
      Sentence(
        ExistentialQuantifier(ProperNoun("John")),
        TransitiveVerb("bought", ExistentialQuantifier(CommonNoun("donkey")))
      ),
      Sentence(
        DefiniteNounPhrase(CommonNoun("animal")),
        VerbInPhrase(
          InPhrase("as", ExistentialQuantifier(CommonNoun("hell"))),
          VerbAdjectivePhrase("was", Adjective("stubborn"))
        )
      )
    ))
    
    parse("John left his wallet on a table").right.value shouldBe Discourse(List(
      Sentence(
        ExistentialQuantifier(ProperNoun("John")),
        VerbInPhrase(
          InPhrase("on", ExistentialQuantifier(CommonNoun("table"))),
          TransitiveVerb(
            "left",
            NounPhrasePreposition(
              PossessionPhrase(PossessivePronoun("his")),
              ExistentialQuantifier(CommonNoun("wallet"))
            )
          )
        )
      )
    ))

  }
}
