package sensala.web.actors

import javax.inject.Inject

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.event.LoggingReceive
import akka.stream.scaladsl._
import akka.pattern.{ask, pipe}
import akka.util.Timeout
import play.api.Configuration
import play.api.libs.concurrent.InjectedActorSupport
import play.api.libs.json._
import sensala.web.actors.UserActor.CreateAdapter

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class UserParentActor @Inject()(
  childFactory: UserActor.Factory,
  configuration: Configuration
)(implicit ec: ExecutionContext)
    extends Actor
    with InjectedActorSupport
    with ActorLogging {

  import UserParentActor._

  implicit private val timeout: Timeout = Timeout(2 seconds)

  override def receive: Receive = LoggingReceive {
    case Create(id) =>
      val name = s"userActor-$id"
      log.info(s"Creating initiator actor $name")
      val child: ActorRef = injectedChild(childFactory(id), name)
      val future          = (child ? CreateAdapter(id)).mapTo[Flow[JsValue, JsValue, _]]
      pipe(future) to sender()
  }
}

object UserParentActor {
  case class Create(id: String)
}
