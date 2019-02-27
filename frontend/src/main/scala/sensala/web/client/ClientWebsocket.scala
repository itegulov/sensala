package sensala.web.client

import org.scalajs.dom._
import org.scalajs.dom.html.{Div, Heading}
import org.singlespaced.d3js.d3
import io.circe.syntax._
import io.circe.parser._
import sensala.models.SensalaNode
import sensala.web.client.MainJS.moveOnZoom
import sensala.web.client.dagre.Dagre
import sensala.models._
import sensala.models.GenericDerivation._

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
      val message = parse(e.data.toString) match {
        case Right(json) =>
          json.as[SensalaInterpretMessage] match {
            case Right(StanfordParsed(result)) =>
              renderText(result, "svg-canvas-stanford")
            case Right(SensalaParsed(result)) =>
              renderText(result, "svg-canvas-sensala")
            case Right(SensalaInterpreted(result)) =>
              termHeading.textContent = result
              loader.style.display = "none"
              termHeading.style.display = "block"
              termHeading.style.color = "black"
            case Right(SensalaError(error)) =>
              termHeading.textContent = error
              loader.style.display = "none"
              termHeading.style.display = "block"
              termHeading.style.color = "red"
            case Right(other) =>
              println(s"Other message: $other")
            case Left(error) =>
              println(error)
          }
        case Left(failure) =>
          println(failure)
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
    socket.send((SensalaRunInterpretation(discourse): SensalaInterpretMessage).asJson.toString())
}
