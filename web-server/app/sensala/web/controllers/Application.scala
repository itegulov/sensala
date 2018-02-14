package sensala.web.controllers

import javax.inject.{Inject, Named}

import akka.NotUsed
import akka.actor._
import akka.pattern.ask
import akka.stream.scaladsl._
import akka.util.Timeout
import com.typesafe.scalalogging.Logger
import org.aossie.scavenger.expression.E
import org.aossie.scavenger.expression.formula.True
import org.aossie.scavenger.preprocessing.TPTPClausifier
import org.aossie.scavenger.structure.immutable.AxiomClause
import org.atnos.eff._
import org.atnos.eff.syntax.all._
import play.api.libs.json._
import play.api.mvc._
import sensala.error.NLError
import sensala.normalization.NormalFormConverter
import sensala.parser.DiscourseParser
import sensala.postprocessing.PrettyTransformer
import sensala.property.{CachedPropertyExtractor, ConceptNetPropertyExtractor}
import sensala.structure._
import sensala.web.actors.UserParentActor
import sensala.web.shared.SensalaInterpretMessage

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class Application @Inject()(@Named("userParentActor") userParentActor: ActorRef)(
  implicit val executionContext: ExecutionContext
) extends InjectedController {
  private val logger             = Logger[this.type]

  def index = Action { implicit request =>
    val debug =
      """
        |{"StanfordParsed":{"result":{"label":"ROOT","color":"white","children":[{"label":"NP","color":"green","children":[{"label":"NNP John","color":"green","children":[]}]}]}}}
      """.stripMargin
    Json.parse(debug).validate[SensalaInterpretMessage] match {
      case JsSuccess(res, _) =>
        println(res)
      case JsError(errors) =>
        println(errors.mkString("\n"))
    }
    Ok(views.html.index())
  }

  private def wsFutureFlow(request: RequestHeader): Future[Flow[JsValue, JsValue, NotUsed]] = {
    // Use guice assisted injection to instantiate and configure the child actor.
    implicit val timeout: Timeout = Timeout(1 second) // the first run in dev can take a while :-(
    val future: Future[Any]       = userParentActor ? UserParentActor.Create(request.id.toString)
    val futureFlow: Future[Flow[JsValue, JsValue, NotUsed]] =
      future.mapTo[Flow[JsValue, JsValue, NotUsed]]
    futureFlow
  }

  def ws: WebSocket = WebSocket.acceptOrResult[JsValue, JsValue] { rh =>
    wsFutureFlow(rh).map { flow =>
      Right(flow)
    }.recover {
      case e: Exception =>
        logger.error("Cannot create websocket", e)
        val jsError = Json.obj("error" -> "Cannot create websocket")
        val result  = InternalServerError(jsError)
        Left(result)
    }
  }
}
