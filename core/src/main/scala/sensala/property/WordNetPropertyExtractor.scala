package sensala.property

import cats.effect.Sync
import cats.implicits._
import net.sf.extjwnl.data.list._
import net.sf.extjwnl.dictionary.Dictionary
import net.sf.extjwnl.data._
import org.aossie.scavenger.expression.Sym
import sensala.shared.effect.Log
import sensala.structure._

import scala.jdk.CollectionConverters._

final case class WordNetPropertyExtractor[F[_]: Sync: Log] private (
  private val dictionary: Dictionary
) {
  private def getHypernymTree(synset: Synset): F[PointerTargetTree] =
    Sync[F].delay { PointerUtils.getHypernymTree(synset) }

  def extractProperties(word: String): F[List[Property]] =
    dictionary.lookupAllIndexWords(word).getIndexWordArray.toList.flatTraverse[F, Property] {
      indexWord =>
        indexWord.getSenses.asScala.to(List).flatTraverse[F, Property] { sense =>
          for {
            _          <- Log[F].debug(s"Trying sense for $word: ${sense.getGloss}")
            treeEither <- getHypernymTree(sense).attempt
            tree       = treeEither.map(tree => tree.toList.asScala.to(List))
            result <- tree.getOrElse(List.empty).flatTraverse[F, Property] { pathAny =>
                       val path = pathAny
                       path.asScala.toList.flatTraverse[F, Property] { nodeAny =>
                         val node       = nodeAny
                         val properties = node.getSynset.getWords.asScala.map(_.getLemma).to(List)
                         Log[F].debug(properties.mkString(", ")) >>
                           properties.map(lemma => Property(x => Sym(lemma)(x))).pure[F]
                       }
                     }
          } yield result
        }
    }
}

object WordNetPropertyExtractor {
  def apply[F[_]](implicit ev: WordNetPropertyExtractor[F]): WordNetPropertyExtractor[F] = ev

  def create[F[_]: Sync: Log](): F[WordNetPropertyExtractor[F]] =
    for {
      dictonary <- Sync[F].delay { Dictionary.getDefaultResourceInstance }
    } yield WordNetPropertyExtractor(dictonary)
}
