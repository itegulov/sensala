package sensala.interpretation

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import org.scalactic.Equality
import sensala.SensalaSpec
import sensala.error.NLError
import sensala.normalization.NormalFormConverter
import sensala.parser.{DiscourseParser, SensalaStanfordParser}
import sensala.postprocessing.PrettyTransformer
import sensala.property.{CachedPropertyExtractor, ConceptNetPropertyExtractor}
import sensala.structure._
import sensala.structure.types._

class InterpretationSpec extends SensalaSpec {
  implicit val propertyExtractor = CachedPropertyExtractor(ConceptNetPropertyExtractor)
  val discourseParser = DiscourseParser()
  
  def interpret(text: String): E = {
    val sentences = SensalaStanfordParser.parse(text)
    val parsed = discourseParser.parse(sentences)
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
        val ((resultEither, _), _) = resultM
          .runEither[NLError]
          .runState[Context](Context(Map.empty, Set.empty))
          .runState[LocalContext](LocalContext.empty)
          .run
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
  val z = Var("z")
  val e = Var("e")
  val eSucc = Var("e'")
  val eSuccSucc = Var("e''")
  
  val named = Sym("named")
  def John(v: Var) = named(v, Sym("John"))
  def Mary(v: Var) = named(v, Sym("Mary"))
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
  val runs = Sym("runs")
  val quickly = Sym("quickly")
  val description = Sym("description")
  val believes = Sym("believes")
  val said = Sym("said")
  val wallet = Sym("wallet")
  val table = Sym("table")
  val on = Sym("on")
  
  def ex(x: Var, e: E): E = Ex(x, entity, e)
  def exEv(x: Var, e: E): E = Ex(x, event, e)
  def forall(x: Var, e: E): E = All(x, entity, e)
  def foallEv(x: Var, e: E): E = All(x, event, e)

  it should "interpret simple sentences" in {
    interpret("John loves Mary") shouldEqual
      ex(x, John(x) /\: ex(y, Mary(y) /\: exEv(e, loves(e) /\: agent(e, x) /\: patient(e, y))))
    interpret("John walks") shouldEqual
      ex(x, John(x) /\: exEv(e, walks(e) /\: agent(e, x)))
  }

  it should "interpret quantified sentences" in {
    interpret("A farmer walks") shouldEqual
      ex(x, farmer(x) /\: exEv(e, walks(e) /\: agent(e, x)))
    interpret("An anthropologist discovered a skeleton") shouldEqual
      ex(x, anthropologist(x) /\: ex(y, skeleton(y) /\: exEv(e, discovered(e) /\: agent(e, x) /\: patient(e, y))))
    interpret("John owns a donkey") shouldEqual
      ex(x, John(x) /\: ex(y, donkey(y) /\: exEv(e, owns(e) /\: agent(e, x) /\: patient(e, y))))
    interpret("Every farmer owns a donkey") shouldEqual
      forall(x, farmer(x) ->: ex(y, donkey(y) /\: exEv(e, owns(e) /\: agent(e, x) /\: patient(e, y))))
  }

  it should "interpret donkey anaphora" in {
    interpret("Every farmer who owns a donkey beats it") shouldEqual
      forall(x, farmer(x) ->: forall(y, donkey(y) ->: foallEv(e, owns(e) ->: agent(e, x) ->: patient(e, y) ->: exEv(e, beats(e) /\: agent(e, x) /\: patient(e, y)))))
    interpret("A farmer who owns a donkey beats it") shouldEqual
      ex(x, farmer(x) /\: ex(y, donkey(y) /\: exEv(e, owns(e) /\: agent(e, x) /\: patient(e, y) /\: exEv(e, beats(e) /\: agent(e, x) /\: patient(e, y)))))
  }

  it should "interpret sentences with adjectives" in {
    interpret("Every wealthy farmer owns a fat donkey") shouldEqual
      forall(x, farmer(x) ->: wealthy(x) ->: ex(y, donkey(y) /\: fat(y) /\: exEv(e, owns(e) /\: agent(e, x) /\: patient(e, y))))
    interpret("Every wealthy farmer who owns a fat donkey beats it") shouldEqual
      forall(x, farmer(x) ->: wealthy(x) ->: forall(y, donkey(y) ->: fat(y) ->: foallEv(e, owns(e) ->: agent(e, x) ->: patient(e, y) ->: exEv(e, beats(e) /\: agent(e, x) /\: patient(e, y)))))
  }

  it should "interpret adjective verb sentences" in {
    interpret("John is smart") shouldEqual
      ex(x, John(x) /\: exEv(e, description(e) /\: smart(e, x)))
  }

  it should "interpret other anaphora sentences" in {
    interpret("Every lawyer believes he is smart") shouldEqual
      forall(x, lawyer(x) ->: exEv(e, exEv(eSucc, description(eSucc) /\: smart(eSucc, x) /\: believes(e) /\: agent(e, x) /\: patient(e, eSucc))))
    interpret("John left. He said he was ill.") shouldEqual
      ex(x, John(x) /\: exEv(e, left(e) /\: agent(e, x) /\: exEv(eSucc, exEv(eSuccSucc, description(eSuccSucc) /\: ill(eSuccSucc, x) /\: said(eSucc) /\: agent(eSucc, x) /\: patient(eSucc, eSuccSucc)))))
  }

  it should "interpret sentences with adverb for verbs" in {
    interpret("John runs quickly") shouldEqual
      ex(x, John(x) /\: exEv(e, runs(e) /\: agent(e, x) /\: quickly(e)))
  }

  it should "interpret sentences with propositions" in {
    interpret("John left a wallet on a table") shouldEqual
      ex(x, John(x) /\: ex(y, wallet(y) /\: exEv(e, left(e) /\: agent(e, x) /\: patient(e, y) /\: ex(z, table(z) /\: on(e, z)))))
  }
}
