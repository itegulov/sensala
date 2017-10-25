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
    interpret("John loves Mary") === AppRec("loves", List("John", "Mary"))
    interpret("John walks") === App("walks", "John")
  }
  
  it should "interpret quantified sentences" in {
    interpret("John walks") === App("walks", "John")
    interpret("A farmer walks") === Ex(x, i, And(App("farmer", x), App("walks", x)))
    interpret("John owns a donkey") === Ex(x, i, And(App("donkey", x), AppRec("owns", List("John", x))))
    interpret("Every farmer owns a donkey") === All(x, i, Imp(App("farmer", x), Ex(y, i, And(App("donkey", y), AppRec("owns", List(x, y))))))
  }
}
