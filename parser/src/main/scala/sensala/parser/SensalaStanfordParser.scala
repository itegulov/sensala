package sensala.parser

import edu.stanford.nlp.ling.CoreAnnotations.SentencesAnnotation
import edu.stanford.nlp.pipeline.{Annotation, StanfordCoreNLP}
import edu.stanford.nlp.trees.Tree
import edu.stanford.nlp.trees.TreeCoreAnnotations.TreeAnnotation
import edu.stanford.nlp.util.CoreMap

import scala.collection.JavaConverters._

object SensalaStanfordParser extends StanfordCoreNLP("stanford_parser.properties") {
  def parse(discourse: String): List[Tree] = {
    val document = new Annotation(discourse)
    SensalaStanfordParser.annotate(document)
    val sentences: List[CoreMap] = document.get(classOf[SentencesAnnotation]).asScala.toList
    sentences.map(_.get(classOf[TreeAnnotation]))
  }
}
