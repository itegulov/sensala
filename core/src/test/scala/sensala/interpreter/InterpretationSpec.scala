package sensala.interpreter

import sensala.models.nl._
import sensala.structure._

class InterpretationSpec extends CommonInterpretationSpec {
  it should "interpret simple sentences" in {
    // John loves Mary
    interpret(
      Sentence(John, TransitiveVerb("loves", Mary))
    ).shouldEqual(
      ex(x, John(x) /\ ex(y, Mary(y) /\ exEv(e, loves(e) /\ agent(e, x) /\ patient(e, y))))
    )
    // John walks
    interpret(
      Sentence(John, IntransitiveVerb("walks"))
    ).shouldEqual(ex(x, John(x) /\ exEv(e, walks(e) /\ agent(e, x))))
  }

  it should "interpret quantified sentences" in {
    // A farmer walks
    interpret(
      Sentence(ExistentialQuantifier(CommonNoun("farmer")), IntransitiveVerb("walks"))
    ).shouldEqual(ex(x, farmer(x) /\ exEv(e, walks(e) /\ agent(e, x))))
    // An anthropologist discovered a skeleton
    interpret(
      Sentence(
        ExistentialQuantifier(CommonNoun("anthropologist")),
        TransitiveVerb("discovered", ExistentialQuantifier(CommonNoun("skeleton")))
      )
    ).shouldEqual(
      ex(
        x,
        anthropologist(x) /\ ex(
          y,
          skeleton(y) /\ exEv(e, discovered(e) /\ agent(e, x) /\ patient(e, y))
        )
      )
    )
    // John owns a donkey
    interpret(
      Sentence(
        John,
        TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ).shouldEqual(
      ex(x, John(x) /\ ex(y, donkey(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y))))
    )
    // Every farmer owns a donkey
    interpret(
      Sentence(
        ForallQuantifier(CommonNoun("farmer")),
        TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey")))
      )
    ).shouldEqual(
      forall(x, farmer(x) ->: ex(y, donkey(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y))))
    )
  }

  it should "interpret donkey anaphora" in {
    // Every farmer who owns a donkey beats it
    interpret(
      Sentence(
        ForallQuantifier(
          WhNounPhrase(
            TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
            CommonNoun("farmer")
          )
        ),
        TransitiveVerb("beats", itPronoun)
      )
    ).shouldEqual(
      forall(
        x,
        farmer(x) ->: forall(
          y,
          donkey(y) ->: foallEv(
            e,
            (owns(e) /\ agent(e, x) /\ patient(e, y)) ->: exEv(
              e,
              beats(e) /\ agent(e, x) /\ patient(e, y)
            )
          )
        )
      )
    )
    // A farmer who owns a donkey beats it
    interpret(
      Sentence(
        ExistentialQuantifier(
          WhNounPhrase(
            TransitiveVerb("owns", ExistentialQuantifier(CommonNoun("donkey"))),
            CommonNoun("farmer")
          )
        ),
        TransitiveVerb("beats", itPronoun)
      )
    ).shouldEqual(
      ex(
        x,
        farmer(x) /\ ex(
          y,
          donkey(y) /\ exEv(
            e,
            owns(e) /\ agent(e, x) /\ patient(e, y) /\ exEv(
              e,
              beats(e) /\ agent(e, x) /\ patient(e, y)
            )
          )
        )
      )
    )
  }

  it should "interpret sentences with adjectives" in {
    // Every wealthy farmer owns a fat donkey
    interpret(
      Sentence(
        ForallQuantifier(AdjectiveNounPhrase(Adjective("wealthy"), CommonNoun("farmer"))),
        TransitiveVerb(
          "owns",
          ExistentialQuantifier(
            AdjectiveNounPhrase(Adjective("fat"), CommonNoun("donkey"))
          )
        )
      )
    ).shouldEqual(
      forall(
        x,
        farmer(x) ->: wealthy(x) ->: ex(
          y,
          donkey(y) /\ (fat(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y)))
        )
      )
    )
    // Every wealthy farmer who owns a fat donkey beats it
    interpret(
      Sentence(
        ForallQuantifier(
          WhNounPhrase(
            TransitiveVerb(
              "owns",
              AdjectiveNounPhrase(
                Adjective("fat"),
                ExistentialQuantifier(CommonNoun("donkey"))
              )
            ),
            AdjectiveNounPhrase(Adjective("wealthy"), CommonNoun("farmer"))
          )
        ),
        TransitiveVerb("beats", itPronoun)
      )
    ).shouldEqual(
      forall(
        x,
        farmer(x) ->: wealthy(x) ->: forall(
          y,
          donkey(y) ->: fat(y) ->: foallEv(
            e,
            (owns(e) /\ agent(e, x) /\ patient(e, y)) ->: exEv(
              e,
              beats(e) /\ agent(e, x) /\ patient(e, y)
            )
          )
        )
      )
    )
  }

  it should "interpret adjective verb sentences" in {
    // John is smart
    interpret(
      Sentence(John, VerbAdjectivePhrase("is", Adjective("smart")))
    ).shouldEqual(ex(x, John(x) /\ exEv(e, description(e) /\ smart(e, x))))
  }

  it should "interpret other anaphora sentences" in {
    // Every lawyer believes he is smart
    interpret(
      Sentence(
        ForallQuantifier(CommonNoun("lawyer")),
        VerbSentencePhrase(
          "believes",
          Sentence(
            he,
            VerbAdjectivePhrase("is", Adjective("smart"))
          )
        )
      )
    ).shouldEqual(
      forall(
        x,
        lawyer(x) ->: exEv(
          e,
          exEv(
            eSucc,
            description(eSucc) /\ smart(eSucc, x) /\ (believes(e) /\ agent(e, x) /\ patient(
              e,
              eSucc
            ))
          )
        )
      )
    )
    // John left. He said he was ill.
    interpret(
      Discourse(
        List(
          Sentence(John, IntransitiveVerb("left")),
          Sentence(
            he,
            VerbSentencePhrase(
              "said",
              Sentence(
                he,
                VerbAdjectivePhrase("was", Adjective("ill"))
              )
            )
          )
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ exEv(
          e,
          left(e) /\ agent(e, x) /\ exEv(
            eSucc,
            exEv(
              eSuccSucc,
              description(eSuccSucc) /\ ill(eSuccSucc, x) /\ (said(eSucc) /\ agent(eSucc, x) /\ patient(
                eSucc,
                eSuccSucc
              ))
            )
          )
        )
      )
    )
  }

  it should "interpret sentences with adverb for verbs" in {
    // John runs quickly
    interpret(
      Sentence(
        John,
        VerbAdverbPhrase(Adverb("quickly"), IntransitiveVerb("runs"))
      )
    ).shouldEqual(
      ex(x, John(x) /\ exEv(e, runs(e) /\ agent(e, x) /\ quickly(e)))
    )
  }

  it should "interpret sentences with propositions" in {
    // John left a wallet on a table
    interpret(
      Sentence(
        John,
        VerbInPhrase(
          InPhrase("on", ExistentialQuantifier(CommonNoun("table"))),
          TransitiveVerb("left", ExistentialQuantifier(CommonNoun("wallet")))
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(
          y,
          wallet(y) /\ exEv(
            e,
            left(e) /\ agent(e, x) /\ patient(e, y) /\ ex(z, table(z) /\ on(e, z))
          )
        )
      )
    )
  }

  it should "interpret sentences with comparatives" in {
    interpret(
      Sentence(
        John,
        VerbComparativePhrase(
          "smarter",
          Bob
        )
      )
    ).shouldEqual(
      ex(
        x,
        John(x) /\ ex(y, Bob(y) /\ COMP(smarter, x, y))
      )
    )
  }
}
