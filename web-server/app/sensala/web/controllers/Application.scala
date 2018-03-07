package sensala.web.controllers

import javax.inject.{Inject, Named}

import akka.NotUsed
import akka.actor._
import akka.pattern.ask
import akka.stream.scaladsl._
import akka.util.Timeout
import com.typesafe.scalalogging.Logger
import play.api.libs.json._
import play.api.mvc._
import sensala.web.actors.UserParentActor

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

class Application @Inject()(@Named("userParentActor") userParentActor: ActorRef)(
  implicit val executionContext: ExecutionContext
) extends InjectedController {
  private val logger = Logger[this.type]

  def index = Action { implicit request =>
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
