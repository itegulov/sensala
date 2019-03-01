package sensala.interpreter

import sensala.models.nl._
import sensala.structure._

class ACL2018SensalaDemoInterpretationSpec extends CommonInterpretationSpec {
  it should "interpret examples from ACL 2018 system demonstration paper" in {
    // John loves Mary
    interpret(
      Sentence(John, TransitiveVerb("loves", Mary))
    ).shouldEqual(
      ex(x, John(x) /\ ex(y, Mary(y) /\ exEv(e, loves(e) /\ agent(e, x) /\ patient(e, y))))
    )

    // John runs quickly
    interpret(
      Sentence(John, VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs")))
    ).shouldEqual(
      ex(x, John(x) /\ exEv(e, runs(e) /\ agent(e, x) /\ quickly(e)))
    )

    // John loves Mary. She believes him.
    interpret(
      Discourse(
        List(
          Sentence(John, TransitiveVerb("loves", Mary)),
          Sentence(She, TransitiveVerb("believes", him))
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          Mary(y) /\ exEv(
            e,
            loves(e) /\ agent(e, x) /\ patient(e, y) /\
              exEv(eSucc, believes(eSucc) /\ agent(eSucc, y) /\ patient(eSucc, x))
          )
        )
      )
    )

    // John loves Mary. She believes this.
    interpret(
      Discourse(
        List(
          Sentence(John, TransitiveVerb("loves", Mary)),
          Sentence(She, TransitiveVerb("believes", DemonstrativePronoun("this")))
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          Mary(y) /\ exEv(
            e,
            loves(e) /\ agent(e, x) /\ patient(e, y) /\
              exEv(eSucc, believes(eSucc) /\ agent(eSucc, y) /\ patient(eSucc, e))
          )
        )
      )
    )

    // John ate a pizza with a fork. Bob did too.
    interpret(
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
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          pizza(y) /\ exEv(
            e,
            ate(e) /\ agent(e, x) /\ patient(e, y) /\ ex(
              z,
              fork(z) /\ (`with`(e, z) /\
                ex(
                  b,
                  Bob(b) /\ exEv(
                    eSucc,
                    ate(eSucc) /\ agent(eSucc, b) /\ patient(eSucc, y) /\ `with`(eSucc, z)
                  )
                ))
            )
          )
        )
      )
    )

    // Mary is loved by John. So is Ann.
    interpret(
      Discourse(
        List(
          Sentence(John, TransitiveVerb("loved", Mary)),
          Sentence(
            Ann,
            VerbPhraseAnaphora("So is", Passive)
          )
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          Mary(y) /\ exEv(
            e,
            loved(e) /\ agent(e, x) /\ patient(e, y) /\
              ex(z, Ann(z) /\ exEv(eSucc, loved(eSucc) /\ agent(eSucc, x) /\ patient(eSucc, z)))
          )
        )
      )
    )

    // Every farmer who owns a donkey thinks he is rich
    interpret(
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
    ).shouldEqual(
      forall(
        x,
        farmer(x) ->: forall(
          y,
          donkey(y) ->: foallEv(
            e,
            (owns(e) /\ agent(e, x) /\ patient(e, y)) ->:
              exEv(
              e,
              exEv(
                eSucc,
                description(eSucc) /\ rich(eSucc, x) /\ (thinks(e) /\ agent(e, x) /\ patient(
                  e,
                  eSucc
                ))
              )
            )
          )
        )
      )
    )

    // John bought a donkey. The animal was stubborn as hell.
    interpret(
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
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          donkey(y) /\ exEv(
            e,
            bought(e) /\ agent(e, x) /\ patient(e, y) /\
              exEv(
                eSucc,
                description(eSucc) /\ stubborn(eSucc, y) /\ ex(z, hell(z) /\ as(eSucc, z))
              )
          )
        )
      )
    )

    // John left his wallet on a table
    interpret(
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
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          wallet(y) /\ (owns(x, y) /\ exEv(
            e,
            left(e) /\ agent(e, x) /\ patient(e, y) /\
              ex(z, table(z) /\ on(e, z))
          ))
        )
      )
    )
  }
}
