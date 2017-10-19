package au.edu.anu.sensala.structure

import cats.data.State

trait Article extends NounPhrase

case class ExistentialArticle(commonNoun: CommonNoun) extends Article {
  override def interpret: CState = for {
    p <- commonNoun.interpret
    q <- bindFreeSym
    x <- bindFreeSym
    _ <- State.modify[Context](_.extend(x))
  } yield Abs(q, Abs(x, App(App(p, x), App(q, x))))
}
