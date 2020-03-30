package sensala.interpreter

import cats.Functor
import cats.effect.IO
import cats.mtl.FunctorRaise
import org.aossie.scavenger.expression.{E, Sym, Var}
import org.aossie.scavenger.expression.formula.{All, Ex, True}
import org.scalactic.Equality
import sensala.SensalaSpec
import sensala.error.NLError
import sensala.normalization.NormalFormConverter
import sensala.postprocessing.PrettyTransformer
import sensala.types._
import sensala.structure._
import sensala.interpreter.context.{Context, LocalContext}
import sensala.models.nl.NL
import sensala.property.{PropertyExtractor, WordNetPropertyExtractor}
import sensala.shared.effect.Log

class CommonInterpretationSpec extends SensalaSpec {
  implicit val log = Log.log[IO]
  implicit val raiseNLError = new FunctorRaise[IO, NLError] {
    override val functor: Functor[IO] = Functor[IO]

    override def raise[A](e: NLError): IO[A] =
      throw new RuntimeException(e.toString)
  }
  implicit val wordNetPropertyExtractor = WordNetPropertyExtractor.create[IO]().unsafeRunSync()
  implicit val propertyExtractor        = PropertyExtractor[IO]()

  def interpret(discourse: NL): E = {
    implicit val sensalaContext: Context[IO]           = Context.initial[IO]
    implicit val sensalaLocalContext: LocalContext[IO] = LocalContext.empty[IO]
    val interpreter                                    = Interpreter[IO]()
    (for {
      lambda     <- interpreter.interpret(discourse, IO.pure(True))
      normalized = NormalFormConverter.normalForm(lambda)
      prettified = PrettyTransformer.transform(normalized)
    } yield prettified).unsafeRunSync()
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
  val smarter         = Sym("smarter")

  val COMP = Sym("COMP")
}
