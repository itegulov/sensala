package sensala.web.client

import org.scalajs.dom._
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

object MainJS {
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
    val parsedTermHeading = document.getElementById("parsed-term") match {
      case heading: Heading =>
        heading
      case other =>
        sys.error(s"Element with ID 'parsed-term' is not a heading, it's $other")
    }
    val csrfTokenField = document.getElementsByName("csrfToken")(0) match {
      case input: Input =>
        input
      case other =>
        sys.error(s"Element with ID 'csrfToken' is not an input, it's $other")
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
    val clientWebsocket = ClientWebsocket(loader, termHeading, parsedTermHeading)
    clientWebsocket.connectWS()
    buttonInterpret.onclick = (_: UIEvent) => {
      loader.style.display = "block"
      termHeading.style.display = "none"
      parsedTermHeading.style.display = "none"
      clientWebsocket.sendInterpretationRequest(discourseBox.value)
    }
  }
}
