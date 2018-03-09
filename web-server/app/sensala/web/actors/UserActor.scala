package sensala.web.actors

import javax.inject.Inject

import akka.NotUsed
import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import akka.event.{LogMarker, MarkerLoggingAdapter}
import akka.stream.scaladsl._
import akka.stream.{Materializer, OverflowStrategy}
import akka.util.Timeout
import com.google.inject.assistedinject.Assisted
import play.api.libs.json._
import sensala.web.actors.UserActor.CreateAdapter

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class UserActor @Inject()(
  @Assisted id: String
)(implicit sys: ActorSystem, mat: Materializer, ec: ExecutionContext)
    extends Actor {
  private val marker = LogMarker(name = self.path.name)
  implicit private val log: MarkerLoggingAdapter =
    akka.event.Logging.withMarker(context.system, this.getClass)
  implicit private val timeout: Timeout = Timeout(50 millis)

  private var clientId = "NOT_SET"

  override def receive: Receive = {
    case CreateAdapter(cId) =>
      clientId = cId
      log.info(s"Create Websocket for Client: $clientId")
      sender() ! websocketFlow
    case other =>
      log.info(s"Unexpected message from ${sender()}: $other")
  }

  override def postStop(): Unit =
    log.info(marker, s"Stopping $clientId: actor $self")

  /**
   * Generates a flow that can be used by the websocket.
   *
   * @return the flow of JSON
   */
  private lazy val websocketFlow: Flow[JsValue, JsValue, NotUsed] = {
    val intepretActor = sys.actorOf(Props(InterpretationActor()))
    val incomingMessages: Sink[JsValue, NotUsed] =
      Flow[JsValue].map { msg =>
        InterpretationActor.IncomingMessage(msg)
      }.to(Sink.actorRef[InterpretationActor.IncomingMessage](intepretActor, PoisonPill))
    val outgoingMessages: Source[JsValue, NotUsed] =
      Source
        .actorRef[InterpretationActor.OutgoingMessage](10, OverflowStrategy.fail)
        .mapMaterializedValue { outActor =>
          // give the user actor a way to send messages out
          intepretActor ! InterpretationActor.Connected(outActor)
          NotUsed
        }
        .map {
          case InterpretationActor.OutgoingMessage(msg) => msg
        }
    Flow.fromSinkAndSource(incomingMessages, outgoingMessages)
  }
}

object UserActor {

  // used to inject the UserActors as childs of the UserParentActor
  trait Factory {
    def apply(id: String): Actor
  }

  case class CreateAdapter(clientId: String)
}
