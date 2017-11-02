package sensala.conceptnet

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.Logger
import play.api.libs.json.{JsError, JsSuccess, JsValue}
import play.api.libs.ws._
import play.api.libs.ws.ahc._
import sensala.conceptnet.structure.{ConceptNetWord, ConceptNetWordPage}
import sensala.conceptnet.structure.json._

import scala.concurrent.Future

class ConceptNetApi(implicit system: ActorSystem, materializer: ActorMaterializer) {
  import JsonBodyReadables._
  import scala.concurrent.ExecutionContext.Implicits._
  
  private val logger = Logger[this.type]
  
  private val wsClient = StandaloneAhcWSClient()
  
  def requestWord(word: String): Future[ConceptNetWord] = {
    requestUrl(s"http://api.conceptnet.io/c/en/$word?limit=100")
  }
  
  private def requestUrl(url: String): Future[ConceptNetWord] = {
    wsClient.url(url)
      .get()
      .flatMap { response =>
        val body = response.body[JsValue]
        body.validate[ConceptNetWordPage] match {
          case JsSuccess(page, _) =>
            logger.debug(s"Parsed page: $page")
            page.view.nextPage match {
              case Some(nextPageUrl) =>
                requestUrl(s"http://api.conceptnet.io$nextPageUrl").map {
                  x => x.copy(edges = page.edges ++ x.edges)
                }
              case None =>
                Future.successful(ConceptNetWord(page.id, page.context, page.edges))
            }
          case JsError(errors) =>
            logger.error(errors.mkString("\n"))
            sys.error("Invalid state")
        }
      }
  }
}
