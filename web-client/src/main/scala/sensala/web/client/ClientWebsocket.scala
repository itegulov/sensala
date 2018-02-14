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
import scala.scalajs.js.annotation.ScalaJSDefined
import scala.scalajs.js.timers.setTimeout
import scala.scalajs.js

case class ClientWebsocket(loader: Div, termHeading: Heading, parsedTermHeading: Heading) {
  private lazy val wsURL = s"ws://${window.location.host}/ws"

  lazy val socket = new WebSocket(wsURL)

  @ScalaJSDefined
  class Node(val id: Int, val label: String, val nodeClass: String) extends js.Object

  @ScalaJSDefined
  class Edge(val source: Int, val target: Int, val id: String) extends js.Object

  private def renderText(tree: StanfordNode): Unit = {
    val nodes = new ArrayBuffer[Node]()
    val edges = new ArrayBuffer[Edge]()

    populate(tree, nodes, edges)

    val g = Dagre.newD3Digraph

    nodes.foreach(node =>
      g.setNode(node.id.toString, js.Dictionary("label" -> node.label, "class" -> node.nodeClass, "rx" -> 5, "ry" -> 5))
    )

    edges.foreach(edge =>
      g.setEdge(edge.source.toString, edge.target.toString, js.Dictionary("lineInterpolate" -> "cardinal"))
    )

    val render = Dagre.newD3Renderer

    d3.select("#svg-canvas g").remove()

    val svg = d3.select("#svg-canvas")
    val svgGroup = svg.append("g")

    render(d3.select("#svg-canvas g"), g)

    svgGroup.attr("transform", "translate(5, 5)")
    moveOnZoom(svg, svgGroup)
  }

  private def populate(tree: StanfordNode, nodes: ArrayBuffer[Node], edges: ArrayBuffer[Edge]): Node = {
    val newNode = new Node(nodes.length, tree.label, tree.nodeType)

    nodes += newNode

    tree.children.foreach(child => {
      val childNode = populate(child, nodes, edges)
      edges += new Edge(newNode.id, childNode.id, newNode.id + "-" + childNode.id)
    })

    newNode
  }


  def connectWS(): Unit = {
    socket.onmessage = {
      (e: MessageEvent) =>
        val message = Json.parse(e.data.toString)
        message.validate[SensalaInterpretMessage] match {
          case JsSuccess(StanfordParsed(result), _) =>
            renderText(result)
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
