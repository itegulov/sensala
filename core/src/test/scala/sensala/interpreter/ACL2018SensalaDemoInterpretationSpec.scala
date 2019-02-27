package sensala.interpreter

import sensala.structure._

class ACL2018SensalaDemoInterpretationSpec extends CommonInterpretationSpec {
  it should "interpret examples from ACL 2018 system demonstration paper" in {
    interpret("John loves Mary").shouldEqual(
      ex(x, John(x) /\ ex(y, Mary(y) /\ exEv(e, loves(e) /\ agent(e, x) /\ patient(e, y))))
    )

    interpret("John runs quickly").shouldEqual(
      ex(x, John(x) /\ exEv(e, runs(e) /\ agent(e, x) /\ quickly(e)))
    )

    interpret("John loves Mary. She believes him.").shouldEqual(
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

    interpret("John loves Mary. She believes this.").shouldEqual(
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

    interpret("John ate a pizza with a fork. Bob did too.").shouldEqual(
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

    interpret("Mary is loved by John. So is Ann.").shouldEqual(
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

    interpret("Every farmer who owns a donkey thinks he is rich").shouldEqual(
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

    interpret("John bought a donkey. The animal was stubborn as hell.").shouldEqual(
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

    interpret("John left his wallet on a table").shouldEqual(
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
