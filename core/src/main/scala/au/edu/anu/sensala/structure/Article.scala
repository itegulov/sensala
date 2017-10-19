package au.edu.anu.sensala.structure

import cats.data.State

trait Article extends NounPhrase

case class ExistentialArticle(commonNoun: CommonNoun) extends Article {
  override def interpret: CState = for {
    p <- commonNoun.interpret
    y = Sym("y")
    _ <- State.modify[Context](_.extend(y))
  } yield {
    Abs(y, App(p, y)).asInstanceOf[L]
  }
}
