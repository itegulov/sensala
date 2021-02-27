package sensala.verbnet

import cats.effect.Sync
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import edu.mit.jverbnet.index.{IVerbIndex, VerbIndex}
import sensala.shared.effect.Log

import scala.jdk.CollectionConverters._

final case class VerbInfo(preVerbRole: String, postVerbRole: String)

class VerbNetExtractor[F[_]: Sync: Log] private (index: IVerbIndex) {
  private val idMap = index
    .iterator()
    .asScala
    .toList
    .collect {
      case verb if verb.isRoot =>
        val id             = verb.getID
        val hyphenLocation = id.indexOf('-')
        val verbSimpleForm = id.substring(0, hyphenLocation)
        verbSimpleForm -> verb
    }
    .toMap

  private val defaultVerbInfo = VerbInfo("agent", "patient")

  def extract(verb: String): F[VerbInfo] =
    idMap.get(verb) match {
      case Some(verbClass) =>
        for {
          frames <- Sync[F].delay(verbClass.getFrames.asScala.toList)
          // TODO: Decide which frame to use based on context
          frame         = frames.head
          syntax        <- Sync[F].delay(frame.getSyntax)
          preVerbDescs  <- Sync[F].delay(syntax.getPreVerbDescriptors.asScala.toList)
          postVerbDescs <- Sync[F].delay(syntax.getPostVerbDescriptors.asScala.toList)
          // TODO: Do not ignore other pre and post verb descriptors
          // TODO: What if there are no pre or post verb descriptors?
          preVerbDesc  = preVerbDescs.head.getValue.toLowerCase
          postVerbDesc = postVerbDescs.head.getValue.toLowerCase
        } yield VerbInfo(preVerbDesc, postVerbDesc)
      case None =>
        Log[F].info(s"Could not find $verb in VerbNet") *>
          defaultVerbInfo.pure[F]
    }
}

object VerbNetExtractor {
  def create[F[_]: Sync: Log](): F[VerbNetExtractor[F]] = {
    val verbnetUrl = getClass.getResource("/verbnet3.2")
    for {
      index <- Sync[F].delay(new VerbIndex(verbnetUrl))
      _     <- Sync[F].delay(index.open)
    } yield new VerbNetExtractor[F](index)
  }

  def apply[F[_]](implicit ev: VerbNetExtractor[F]): VerbNetExtractor[F] = ev
}
