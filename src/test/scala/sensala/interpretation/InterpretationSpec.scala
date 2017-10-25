package sensala.interpretation

import org.aossie.scavenger.expression._
import org.aossie.scavenger.expression.formula._
import org.scalactic.Equality
import sensala.SensalaSpec
import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.structure._

class InterpretationSpec extends SensalaSpec {
  def interpret(text: String): E = {
    val parsed = DiscourseParser.parse(text)
    println(parsed)
    val resultM = for {
      lambda <- parsed.interpret
      normalized <- NormalFormConverter.normalForm(lambda)
      converted <- applyConversions(normalized)
      prettified = PrettyTransformer.transform(converted)
    } yield prettified
    val (_, result) = resultM.run(Context(Nil, Set.empty, Nil)).value
    println(result.pretty)
    result
  }
  
  implicit def stringToSym(s: String): Sym = Sym(s)
  
  // Scalatest treats === as an alpha equality
  implicit val lambdaEq: Equality[E] =
    (a: E, b: Any) => b match {
      case bl: E => a =+= bl
      case _ => false
    }
  
  val x = Var("x")
  val y = Var("y")

  it should "interpret simple sentences" in {
    interpret("John loves Mary") shouldEqual Ex(x, i, And(App("John", x), Ex(y, i, And(App("Mary", y), AppRec("loves", List(x, y))))))
    interpret("John walks") shouldEqual Ex(x, i, And(App("John", x), App("walks", x)))
  }
  
  it should "interpret quantified sentences" in {
    interpret("John walks") shouldEqual Ex(x, i, And(App("John", x), App("walks", x)))
    interpret("A farmer walks") shouldEqual Ex(x, i, And(App("farmer", x), App("walks", x)))
    interpret("John owns a donkey") shouldEqual Ex(x, i, And(App("John", x), Ex(y, i, And(App("donkey", y), AppRec("owns", List(x, y))))))
    interpret("Every farmer owns a donkey") shouldEqual All(x, i, Imp(App("farmer", x), Ex(y, i, And(App("donkey", y), AppRec("owns", List(x, y))))))
  }
  
  it should "interpret donkey anaphora" in {
    interpret("Every farmer who owns a donkey beats it") shouldEqual All(x, i, Imp(App("farmer", x), All(y, i, Imp(App("donkey", y), Imp(AppRec("owns", List(x, y)), AppRec("beats", List(x, y)))))))
    interpret("A farmer who owns a donkey beats it") shouldEqual Ex(x, i, And(App("farmer", x), Ex(y, i, And(App("donkey", y), And(AppRec("owns", List(x, y)), AppRec("beats", List(x, y)))))))
  }
  
  it should "interpret sentences with adjectives" in {
    interpret("Every wealthy farmer owns a fat donkey") shouldEqual
      All(x, i, Imp(App("farmer", x), Imp(App("wealthy", x), Ex(y, i, And(App("donkey", y), And(App("fat", y), AppRec("owns", List(x, y))))))))
    interpret("Every wealthy farmer who owns a fat donkey beats it") shouldEqual
      All(x, i, Imp(App("farmer", x), Imp(App("wealthy", x), All(y, i, Imp(App("donkey", y), Imp(App("fat", y), Imp(AppRec("owns", List(x, y)), AppRec("beats", List(x, y)))))))))
  }
  
  it should "interpret adjective verb sentences" in {
    interpret("John is smart") shouldEqual Ex(x, i, And(App("John", x), App("smart", x)))
  }
  
  it should "interpret other anaphora sentences" in {
    interpret("Every lawyer believes he is smart") shouldEqual All(x, i, Imp(App("lawyer", x), App("smart", x)))
  }
}
