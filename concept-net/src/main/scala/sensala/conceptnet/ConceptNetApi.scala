package sensala.conceptnet

import akka.stream.ActorMaterializer
import com.typesafe.scalalogging.Logger
import play.api.libs.json.{JsError, JsSuccess, JsValue}
import play.api.libs.ws._
import play.api.libs.ws.ahc._
import sensala.conceptnet.structure.{ConceptNetWord, ConceptNetWordPage}
import sensala.conceptnet.structure.json._
import org.ehcache.config.builders.CacheConfigurationBuilder
import org.ehcache.config.builders.CacheManagerBuilder
import org.ehcache.config.builders.ResourcePoolsBuilder

import scala.concurrent.Future

class ConceptNetApi(implicit materializer: ActorMaterializer) {

  import JsonBodyReadables._
  import scala.concurrent.ExecutionContext.Implicits._

  private val logger = Logger[this.type]

  private val wsClient = StandaloneAhcWSClient()

  private val cacheManager = CacheManagerBuilder.newCacheManagerBuilder
    .withCache(
      "preConfigured",
      CacheConfigurationBuilder
        .newCacheConfigurationBuilder(
          classOf[String],
          classOf[ConceptNetWord],
          ResourcePoolsBuilder.heap(100)
        )
        .build
    )
    .build(true)

  private val preConfigured =
    cacheManager.getCache("preConfigured", classOf[String], classOf[ConceptNetWord])

  private val cache = cacheManager.createCache(
    "myCache",
    CacheConfigurationBuilder
      .newCacheConfigurationBuilder(
        classOf[String],
        classOf[ConceptNetWord],
        ResourcePoolsBuilder.heap(100)
      )
      .build
  )

  def requestWord(word: String, limit: Int = 5000): Future[ConceptNetWord] =
    requestUrl(s"http://api.conceptnet.io/c/en/$word?limit=100", limit)

  private def requestUrl(url: String, limit: Int): Future[ConceptNetWord] =
    Option(cache.get(url)) match {
      case Some(result) => Future.successful(result)
      case None =>
        wsClient
          .url(url)
          .get()
          .flatMap { response =>
            val body = response.body[JsValue]
            body.validate[ConceptNetWordPage] match {
              case JsSuccess(page, _) =>
                logger.debug(s"Parsed page: $page")
                if (page.edges.size >= limit) {
                  Future.successful(ConceptNetWord(page.id, page.context, page.edges))
                } else {
                  page.view match {
                    case Some(view) =>
                      view.nextPage match {
                        case Some(nextPageUrl) =>
                          requestUrl(
                            s"http://api.conceptnet.io$nextPageUrl",
                            limit - page.edges.size
                          ).map { x =>
                            x.copy(edges = page.edges ++ x.edges)
                          }
                        case None =>
                          Future.successful(ConceptNetWord(page.id, page.context, page.edges))
                      }
                    case None =>
                      Future.successful(ConceptNetWord(page.id, page.context, page.edges))
                  }
                }
              case JsError(errors) =>
                logger.error(errors.mkString("\n"))
                sys.error("Invalid state")
            }
          }
          .map { word =>
            // FIXME: Easily could be a race condition
            cache.put(url, word)
            word
          }
    }

  def close(): Unit = {
    wsClient.close()
    cacheManager.close()
  }
}
