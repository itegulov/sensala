package sensala.interpreter

import cats.Functor
import cats.mtl.FunctorRaise
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.aossie.scavenger.expression.{E, Sym, Var}
import org.aossie.scavenger.expression.formula.{All, Ex, True}
import org.scalactic.Equality
import sensala.SensalaSpec
import sensala.error.NLError
import sensala.interpreter.Interpreter
import sensala.normalization.NormalFormConverter
import sensala.parser.english.EnglishDiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.types._
import sensala.structure._
import sensala.interpreter.context.{Context, LocalContext}
import sensala.property.PropertyExtractor

class CommonInterpretationSpec extends SensalaSpec {
  implicit val raiseNLError = new FunctorRaise[Task, NLError] {
    override val functor: Functor[Task] = Functor[Task]

    override def raise[A](e: NLError): Task[A] =
      throw new RuntimeException(e.toString)
  }
  implicit val propertyExtractor = PropertyExtractor[Task]()

  def interpret(text: String): E =
    EnglishDiscourseParser.parse(text) match {
      case Left(error) =>
        sys.error(error)
      case Right(sentence) =>
        println(s"Sentence: $sentence")
        implicit val sensalaContext: Context[Task]           = Context.initial[Task]
        implicit val sensalaLocalContext: LocalContext[Task] = LocalContext.empty[Task]
        val interpreter                                      = Interpreter[Task]()
        (for {
          lambda     <- interpreter.interpret(sentence, Task.pure(True))
          normalized = NormalFormConverter.normalForm(lambda)
          prettified = PrettyTransformer.transform(normalized)
        } yield prettified).runSyncUnsafe()
    }

  // Scalatest treats === as an alpha equality
  implicit val lambdaEq: Equality[E] =
    (a: E, b: Any) =>
      b match {
        case bl: E => a =+= bl
        case _     => false
    }

  def ex(x: Var, e: E): E      = Ex(x, entity, e)
  def exEv(x: Var, e: E): E    = Ex(x, event, e)
  def forall(x: Var, e: E): E  = All(x, entity, e)
  def foallEv(x: Var, e: E): E = All(x, event, e)

  val x         = Var("x")
  val y         = Var("y")
  val z         = Var("z")
  val b         = Var("b")
  val c         = Var("c")
  val e         = Var("e")
  val eSucc     = Var("e'")
  val eSuccSucc = Var("e''")

  val named           = Sym("named")
  def John(v: Var): E = named(v, Sym("John"))
  def Bob(v: Var): E  = named(v, Sym("Bob"))
  def Mary(v: Var): E = named(v, Sym("Mary"))
  def Ann(v: Var): E  = named(v, Sym("Ann"))
  val walks           = Sym("walks")
  val loves           = Sym("loves")
  val farmer          = Sym("farmer")
  val anthropologist  = Sym("anthropologist")
  val discovered      = Sym("discovered")
  val skeleton        = Sym("skeleton")
  val owns            = Sym("owns")
  val beats           = Sym("beats")
  val donkey          = Sym("donkey")
  val wealthy         = Sym("wealthy")
  val fat             = Sym("fat")
  val smart           = Sym("smart")
  val lawyer          = Sym("lawyer")
  val left            = Sym("left")
  val ill             = Sym("ill")
  val runs            = Sym("runs")
  val quickly         = Sym("quickly")
  val description     = Sym("description")
  val believes        = Sym("believes")
  val said            = Sym("said")
  val wallet          = Sym("wallet")
  val table           = Sym("table")
  val on              = Sym("on")
  val pizza           = Sym("pizza")
  val ate             = Sym("ate")
  val fork            = Sym("fork")
  val `with`          = Sym("with")
  val loved           = Sym("loved")
  val thinks          = Sym("thinks")
  val rich            = Sym("rich")
  val bought          = Sym("bought")
  val stubborn        = Sym("stubborn")
  val hell            = Sym("hell")
  val as              = Sym("as")
}
