package sensala.web.client

import org.scalajs.dom._
import org.scalajs.dom.html._
import org.singlespaced.d3js.d3
import sensala.web.client.dagre.Graph

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

object MainJS {
  @JSName("moveOnZoom")
  @js.native
  object moveOnZoom extends js.Object {
    def apply(svg: js.Any, svgGroup: js.Any, graph: Graph): Unit = js.native
  }
  
  def main(args: Array[String]): Unit = {
    val discourseBox = document.getElementById("discourse") match {
      case input: Input =>
        input
      case other =>
        sys.error(s"Element with ID 'discourse' is not an input, it's $other")
    }
    val termHeading = document.getElementById("term") match {
      case heading: Heading =>
        heading
      case other =>
        sys.error(s"Element with ID 'term' is not a heading, it's $other")
    }
    val buttonInterpret = document.getElementById("interpret") match {
      case button: Button =>
        button
      case other =>
        sys.error(s"Element with ID 'interpret' is not a button, it's $other")
    }
    val loader = document.getElementById("loader") match {
      case div: Div =>
        div
      case other =>
        sys.error(s"Element with ID 'loader' is not a div, it's $other")
    }
    val clientWebsocket = ClientWebsocket(loader, termHeading)
    clientWebsocket.connectWS()
    buttonInterpret.onclick = (_: UIEvent) => {
      d3.select("#svg-canvas-stanford").selectAll("*").remove()
      d3.select("#svg-canvas-sensala").selectAll("*").remove()
      loader.style.display = "block"
      termHeading.style.display = "none"
      clientWebsocket.sendInterpretationRequest(discourseBox.value)
    }
  }
}
