package sensala.property

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.aossie.scavenger.expression.Sym
import sensala.conceptnet.ConceptNetApi
import sensala.conceptnet.structure.auxilary.{Antonym, English, RelatedTo}

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object PropertyExtractor {
  import scala.concurrent.ExecutionContext.Implicits.global

  private implicit val system       = ActorSystem()
  private implicit val materializer = ActorMaterializer()

  private val conceptNetApi = new ConceptNetApi()

  private def normalizeProperty(phrase: String): String = phrase.replace(' ', '_')

  def extractProperties(word: String, maxLevel: Int = 2): List[Property] =
    Await.result(
      conceptNetApi.requestWord(word).map { conceptNetWord =>
        val relatedProperties = conceptNetWord.edges
          .filter(_.end.language == English)
          .filter(_.relation == RelatedTo)
          .map { edge =>
            Property(Sym(normalizeProperty(edge.end.label)))
          }
          .distinct
        val antonymProperties = conceptNetWord.edges
          .filter(x => x.start.language == English && x.end.language == English)
          .filter(_.relation == Antonym)
          .flatMap { edge =>
            List(
              Property(Sym("non_" + normalizeProperty(edge.start.label))),
              Property(Sym("non_" + normalizeProperty(edge.end.label)))
            )
          }
          .distinct
        val properties = (relatedProperties ++ antonymProperties).distinct
        if (maxLevel == 1)
          properties
        else
          properties ++ extractProperties(properties(1).symbol.name, maxLevel - 1)
      },
      Duration.Inf
    )

  def close(): Unit = {
    conceptNetApi.close()
    materializer.shutdown()
    system.terminate()
  }
}
