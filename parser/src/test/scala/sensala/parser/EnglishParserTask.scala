package sensala.parser

import cats.Functor
import cats.mtl.FunctorRaise
import monix.eval.Task
import sensala.error.NLError
import sensala.parser.english.EnglishDiscourseParser
import sensala.structure.Discourse
import sensala.structure.context.{Context, LocalContext}

object EnglishParserTask {
  implicit val raiseNLError = new FunctorRaise[Task, NLError] {
    override val functor: Functor[Task] = Functor[Task]

    override def raise[A](e: NLError): Task[A] =
      throw new RuntimeException(e.toString)
  }
  implicit val context = Context.initial[Task]
  implicit val localContext = LocalContext.empty[Task]
  val parser = EnglishDiscourseParser[Task]()
  
  def parse(discourse: String): Either[String, Discourse[Task]] = {
    parser.parse(discourse)
  }
}
