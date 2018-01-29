package sensala.interpretation

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.scalactic.Equality
import sensala.SensalaSpec
import sensala.error.NLError
import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.structure._

class InterpretationSpec extends SensalaSpec {
  def interpret(text: String): E = {
    val parsed = DiscourseParser.parse(text)
    parsed match {
      case Left(error) =>
        sys.error(error)
      case Right(sentence) =>
        println(s"Sentence: $sentence")
        val resultM = for {
          lambda <- sentence.interpret(Eff.pure(True))
          normalized = NormalFormConverter.normalForm(lambda)
          prettified = PrettyTransformer.transform(normalized)
        } yield prettified
        val (resultEither, _) = resultM.runEither[NLError].runState[Context](Context(Map.empty, Set.empty)).run
        val result = resultEither.right.get
        println(result.pretty)
        result   
    }
  }
  
  // Scalatest treats === as an alpha equality
  implicit val lambdaEq: Equality[E] =
    (a: E, b: Any) => 
      b match {
        case bl: E => a =+= bl
        case _ => false
      }
  
  val x = Var("x")
  val y = Var("y")
  
  val John = Sym("John")
  val Mary = Sym("Mary")
  val walks = Sym("walks")
  val loves = Sym("loves")
  val farmer = Sym("farmer")
  val anthropologist = Sym("anthropologist")
  val discovered = Sym("discovered")
  val skeleton = Sym("skeleton")
  val owns = Sym("owns")
  val beats = Sym("beats")
  val donkey = Sym("donkey")
  val wealthy = Sym("wealthy")
  val fat = Sym("fat")
  val smart = Sym("smart")
  val lawyer = Sym("lawyer")
  val left = Sym("left")
  val ill = Sym("ill")

  it should "interpret simple sentences" in {
    interpret("John loves Mary") shouldEqual
      Exist(x, John(x) /\: Exist(y, Mary(y) /\: loves(x, y)))
    interpret("John walks") shouldEqual
      Exist(x, John(x) /\: walks(x))
  }

  it should "interpret quantified sentences" in {
    interpret("John walks") shouldEqual
      Exist(x, John(x) /\: walks(x))
    interpret("A farmer walks") shouldEqual
      Exist(x, farmer(x) /\: walks(x))
    interpret("An anthropologist discovered a skeleton") shouldEqual
      Exist(x, anthropologist(x) /\: Exist(y, skeleton(y) /\: discovered(x, y)))
    interpret("John owns a donkey") shouldEqual
      Exist(x, John(x) /\: Exist(y, donkey(y) /\: owns(x, y)))
    interpret("Every farmer owns a donkey") shouldEqual
      Forall(x, farmer(x) ->: Exist(y, donkey(y) /\: owns(x, y)))
  }

  it should "interpret donkey anaphora" in {
    interpret("Every farmer who owns a donkey beats it") shouldEqual
      Forall(x, farmer(x) ->: Forall(y, donkey(y) ->: owns(x, y) ->: beats(x, y)))
    interpret("A farmer who owns a donkey beats it") shouldEqual
      Exist(x, farmer(x) /\: Exist(y, donkey(y) /\: owns(x, y) /\: beats(x, y)))
  }

  it should "interpret sentences with adjectives" in {
    interpret("Every wealthy farmer owns a fat donkey") shouldEqual
      Forall(x, farmer(x) ->: wealthy(x) ->: Exist(y, donkey(y) /\: fat(y) /\: owns(x, y)))
    interpret("Every wealthy farmer who owns a fat donkey beats it") shouldEqual
      Forall(x, farmer(x) ->: wealthy(x) ->: Forall(y, donkey(y) ->: fat(y) ->: owns(x, y) ->: beats(x, y)))
  }

  it should "interpret adjective verb sentences" in {
    interpret("John is smart") shouldEqual
      Exist(x, John(x) /\: smart(x))
  }

  it should "interpret other anaphora sentences" in {
    interpret("Every lawyer believes he is smart") shouldEqual
      Forall(x, lawyer(x) ->: smart(x))
    interpret("John left. He said he was ill.") shouldEqual
      Exist(x, John(x) /\: left(x) /\: ill(x))
  }
}
