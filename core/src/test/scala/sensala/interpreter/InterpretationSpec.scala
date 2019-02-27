package sensala.interpreter

import sensala.structure._

class InterpretationSpec extends CommonInterpretationSpec {
  it should "interpret simple sentences" in {
    interpret("John loves Mary").shouldEqual(
      ex(x, John(x) /\ ex(y, Mary(y) /\ exEv(e, loves(e) /\ agent(e, x) /\ patient(e, y))))
    )
    interpret("John walks").shouldEqual(ex(x, John(x) /\ exEv(e, walks(e) /\ agent(e, x))))
  }

  it should "interpret quantified sentences" in {
    interpret("A farmer walks").shouldEqual(ex(x, farmer(x) /\ exEv(e, walks(e) /\ agent(e, x))))
    interpret("An anthropologist discovered a skeleton").shouldEqual(
      ex(
        x,
        anthropologist(x) /\ ex(
          y,
          skeleton(y) /\ exEv(e, discovered(e) /\ agent(e, x) /\ patient(e, y))
        )
      )
    )
    interpret("John owns a donkey").shouldEqual(
      ex(x, John(x) /\ ex(y, donkey(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y))))
    )
    interpret("Every farmer owns a donkey").shouldEqual(
      forall(x, farmer(x) ->: ex(y, donkey(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y))))
    )
  }

  it should "interpret donkey anaphora" in {
    interpret("Every farmer who owns a donkey beats it").shouldEqual(
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
    interpret("A farmer who owns a donkey beats it").shouldEqual(
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
    interpret("Every wealthy farmer owns a fat donkey").shouldEqual(
      forall(
        x,
        farmer(x) ->: wealthy(x) ->: ex(
          y,
          donkey(y) /\ (fat(y) /\ exEv(e, owns(e) /\ agent(e, x) /\ patient(e, y)))
        )
      )
    )
    interpret("Every wealthy farmer who owns a fat donkey beats it").shouldEqual(
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
    interpret("John is smart").shouldEqual(ex(x, John(x) /\ exEv(e, description(e) /\ smart(e, x))))
  }

  it should "interpret other anaphora sentences" in {
    interpret("Every lawyer believes he is smart").shouldEqual(
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
    interpret("John left. He said he was ill.").shouldEqual(
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
    interpret("John runs quickly").shouldEqual(
      ex(x, John(x) /\ exEv(e, runs(e) /\ agent(e, x) /\ quickly(e)))
    )
  }

  it should "interpret sentences with propositions" in {
    interpret("John left a wallet on a table").shouldEqual(
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
}
