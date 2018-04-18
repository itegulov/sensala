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
  
  val John = ExistentialQuantifier(ProperNoun("John", Some(Person), Some(Male)))
  val Bob  = ExistentialQuantifier(ProperNoun("Bob", Some(Person), Some(Male)))
  val Mary = ExistentialQuantifier(ProperNoun("Mary", Some(Person), Some(Female)))
  val Ann  = ExistentialQuantifier(ProperNoun("Ann", Some(Person), Some(Female)))
  
  it should "parse example from ACL 2018 system demonstration paper" in {
    parse("John loves Mary").right.value shouldBe Discourse(List(
      Sentence(John, TransitiveVerb("loves", Mary))
    ))
    
    parse("John runs quickly").right.value shouldBe Discourse(List(
      Sentence(John, VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs")))
    ))
    
    parse("John loves Mary. She believes him.").right.value shouldBe Discourse(List(
      Sentence(John, TransitiveVerb("loves", Mary)),
      Sentence(ReflexivePronoun("She"), TransitiveVerb("believes", ReflexivePronoun("him")))
    ))

    parse("John ate a pizza with a fork. Bob did too.").right.value shouldBe Discourse(List(
      Sentence(
        John,
        VerbInPhrase(
          InPhrase("with", ExistentialQuantifier(CommonNoun("fork"))),
          TransitiveVerb("ate", ExistentialQuantifier(CommonNoun("pizza")))
        )
      ),
      Sentence(
        Bob,
        VerbPhraseAnaphora("did too")
      )
    ))

    parse("Mary is loved by John. So is Ann.").right.value shouldBe Discourse(List(
      Sentence(John, TransitiveVerb("loved", Mary)),
      Sentence(
        Ann,
        VerbPhraseAnaphora("So is")
      )
    ))

    parse("Every farmer who owns a donkey thinks he is rich").right.value shouldBe Discourse(List(
      Sentence(
        ForallQuantifier(WhNounPhrase(
          TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
          CommonNoun("farmer")
        )),
        VerbSentencePhrase(
          "thinks",
          Sentence(ReflexivePronoun("he"), VerbAdjectivePhrase("is", Adjective("rich")))
        )
      )
    ))
    
    parse("John bought a donkey. The animal was stubborn as hell.").right.value shouldBe
      Discourse(List(
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
      ))
    
    parse("John left his wallet on a table").right.value shouldBe Discourse(List(
      Sentence(
        John,
        VerbInPhrase(
          InPhrase("on", ExistentialQuantifier(CommonNoun("table"))),
          TransitiveVerb(
            "left",
            ExistentialQuantifier(NounPhrasePreposition(
              PossessionPhrase(PossessivePronoun("his")),
              CommonNoun("wallet")
            ))
          )
        )
      )
    ))

  }
}
