package sensala.web.client

import org.scalajs.dom._
import org.scalajs.dom.html.{Div, Heading}
import org.singlespaced.d3js.d3
import play.api.libs.json._
import sensala.web.client.MainJS.moveOnZoom
import sensala.web.client.dagre.Dagre
import sensala.web.shared._
import sensala.web.shared.SensalaInterpretMessage._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.timers.setTimeout
import scala.scalajs.js
import scala.scalajs.js.annotation.ScalaJSDefined

case class ClientWebsocket(loader: Div, termHeading: Heading) {
  private lazy val wsURL = s"ws://${window.location.host}/ws"

  var socket: WebSocket = _

  @ScalaJSDefined
  class DagreNode(val id: Int, val label: String, val nodeClass: String) extends js.Object

  @ScalaJSDefined
  class DagreEdge(val source: Int, val target: Int, val id: String) extends js.Object

  private def renderText(tree: SensalaNode, id: String): Unit = {
    val nodes = new ArrayBuffer[DagreNode]()
    val edges = new ArrayBuffer[DagreEdge]()

    populate(tree, nodes, edges)

    val g = Dagre.newD3Digraph

    nodes.foreach(
      node =>
        g.setNode(
          node.id.toString,
          js.Dictionary(
            "label" -> node.label,
            "class" -> node.nodeClass,
            "rx"    -> 3,
            "ry"    -> 3
          )
      )
    )

    edges.foreach(
      edge =>
        g.setEdge(
          edge.source.toString,
          edge.target.toString,
          js.Dictionary("lineInterpolate" -> "cardinal")
      )
    )

    val render = Dagre.newD3Renderer

    d3.select(s"#$id g").remove()

    val svg      = d3.select(s"#$id")
    val svgGroup = svg.append("g")

    render(d3.select(s"#$id g"), g)
    moveOnZoom(svg, svgGroup, g)
  }

  private def populate(tree: SensalaNode,
                       nodes: ArrayBuffer[DagreNode],
                       edges: ArrayBuffer[DagreEdge]): DagreNode = {
    val newNode = new DagreNode(nodes.length, tree.label, tree.nodeType)

    nodes += newNode

    tree.children.foreach(child => {
      val childNode = populate(child, nodes, edges)
      edges += new DagreEdge(newNode.id, childNode.id, newNode.id + "-" + childNode.id)
    })

    newNode
  }

  def connectWS(): Unit = {
    socket = new WebSocket(wsURL)
    socket.onmessage = { (e: MessageEvent) =>
      val message = Json.parse(e.data.toString)
      message.validate[SensalaInterpretMessage] match {
        case JsSuccess(StanfordParsed(result), _) =>
          renderText(result, "svg-canvas-stanford")
        case JsSuccess(SensalaParsed(result), _) =>
          renderText(result, "svg-canvas-sensala")
        case JsSuccess(SensalaInterpreted(result), _) =>
          termHeading.textContent = result
          loader.style.display = "none"
          termHeading.style.display = "block"
          termHeading.style.color = "black"
        case JsSuccess(SensalaError(error), _) =>
          termHeading.textContent = error
          loader.style.display = "none"
          termHeading.style.display = "block"
          termHeading.style.color = "red"
        case JsSuccess(other, _) =>
          println(s"Other message: $other")
        case JsError(errors) =>
          errors.foreach(println)
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

  def sendInterpretationRequest(discourse: String): Unit =
    socket.send(Json.toJson(SensalaRunInterpretation(discourse)).toString())
}
