package sensala.web.client

import org.scalajs.dom._
import org.scalajs.dom.html.{Div, Heading}
import play.api.libs.json._
import sensala.web.shared._
import sensala.web.shared.SensalaInterpretMessage._

import scala.scalajs.js.timers.setTimeout

case class ClientWebsocket(loader: Div, termHeading: Heading, parsedTermHeading: Heading) {
  private lazy val wsURL = s"ws://${window.location.host}/ws"

  lazy val socket = new WebSocket(wsURL)

  def connectWS(): Unit = {
    socket.onmessage = {
      (e: MessageEvent) =>
        val message = Json.parse(e.data.toString)
        message.validate[SensalaInterpretMessage] match {
          case JsSuccess(SensalaParsed(result), _) =>
            parsedTermHeading.textContent = result
            parsedTermHeading.style.display = "block"
          case JsSuccess(SensalaInterpreted(result), _) =>
            termHeading.textContent = result
            loader.style.display = "none"
            termHeading.style.display = "block"
          case JsSuccess(other, _) =>
            println(s"Other message: $other")
          case JsSuccess(SensalaError(error), _) =>
            println(s"Error: $error")
          case JsError(errors) =>
            errors foreach println
        }
    }
    socket.onerror = { (e: Event) =>
      println(s"Exception with websocket: ${e.asInstanceOf[ErrorEvent].message}!")
      socket.close(0, e.asInstanceOf[ErrorEvent].message)
    }
    socket.onopen = { (_: Event) =>
      println("Websocket open!")
    }
    socket.onclose = { (e: CloseEvent) =>
      println("Closed socket" + e.reason)
      setTimeout(1000) {
        connectWS() // try to reconnect automatically
      }
    }
  }
  
  def sendInterpretationRequest(discourse: String): Unit = {
    socket.send(Json.toJson(SensalaRunInterpretation(discourse)).toString())
  }
}
