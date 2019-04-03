package sensala.web.client

import fr.hmil.roshttp.{HttpRequest, Method}
import org.scalajs.dom._
import org.scalajs.dom.html._
import org.singlespaced.d3js.d3
import sensala.web.client.dagre.{Dagre, Graph}
import io.circe.parser._
import monix.execution.Scheduler.Implicits.global
import sensala.models._
import sensala.models.nl._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSGlobal, ScalaJSDefined}
import scala.util.{Failure, Success, Try}

object MainJS {
  @js.native
  @JSGlobal("moveOnZoom")
  object moveOnZoom extends js.Object {
    def apply(svg: js.Any, svgGroup: js.Any, graph: Graph): Unit = js.native
  }

  @ScalaJSDefined
  class DagreNode(val id: Int, val label: String, val nodeClass: String) extends js.Object

  @ScalaJSDefined
  class DagreEdge(val source: Int, val target: Int, val id: String) extends js.Object

  private def renderText(tree: SensalaNode, id: String): Unit = {
    val nodes = new ArrayBuffer[DagreNode]()
    val edges = new ArrayBuffer[DagreEdge]()

    populate(tree, nodes, edges)
    renderText(nodes, edges, id)
  }

  private def renderText(nl: NL, id: String): Unit = {
    val nodes = new ArrayBuffer[DagreNode]()
    val edges = new ArrayBuffer[DagreEdge]()

    populate(nl, nodes, edges)
    renderText(nodes, edges, id)
  }

  private def renderText(nodes: ArrayBuffer[DagreNode],
                         edges: ArrayBuffer[DagreEdge],
                         id: String): Unit = {
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

  private def populate(
    nl: NL,
    nodes: ArrayBuffer[DagreNode],
    edges: ArrayBuffer[DagreEdge]
  ): DagreNode = {
    val newNode = new DagreNode(nodes.length, nl.getClass.getSimpleName, nl.getClass.getSimpleName)

    nodes += newNode

    def processWord(word: String): Unit = {
      val childNode = new DagreNode(nodes.length, word, "word")
      nodes += childNode
      edges += new DagreEdge(newNode.id, childNode.id, newNode.id + "-" + childNode.id)
    }

    val children: List[NL] = nl match {
      case Discourse(sentences) => sentences
      case Sentence(nounPhrase, verbPhrase) =>
        List(nounPhrase, verbPhrase)
      case WhNounPhrase(verbPhrase, nounPhrase) =>
        List(verbPhrase, nounPhrase)
      case IntransitiveVerb(word) =>
        processWord(word)
        List.empty
      case TransitiveVerb(word, obj) =>
        processWord(word)
        List(obj)
      case VerbAdjectivePhrase(verb, adj) =>
        processWord(verb)
        processWord(adj.word)
        List.empty
      case VerbInPhrase(preposition, vp) =>
        List(preposition, vp)
      case VerbPhraseAnaphora(phrase, voice) =>
        processWord(phrase)
        processWord(voice.toString)
        List.empty
      case VerbSentencePhrase(word, sentence) =>
        processWord(word)
        List(sentence)
      case VerbAdverbPhrase(adverb, verbPhrase) =>
        processWord(adverb.word)
        List(verbPhrase)
      case VerbComparativePhrase(comparative, obj) =>
        processWord(comparative)
        List(obj)
      case VerbAdverbialClausePhrase(mark, clause, mods, vp) =>
        processWord(mark)
        mods.foreach(mod => processWord(mod.word))
        List(clause, vp)
      case InPhrase(verbWord, nounPhrase) =>
        processWord(verbWord)
        List(nounPhrase)
      case PossessionPhrase(nounPhrase) =>
        List(nounPhrase)
      case WhNounPhrase(verbPhrase, nounPhrase) =>
        List(verbPhrase, nounPhrase)
      case ProperNoun(word, typ, gender) =>
        processWord(s"$word $typ $gender")
        List.empty
      case CommonNoun(word) =>
        processWord(word)
        List.empty
      case PluralCommonNoun(word) =>
        processWord(word)
        List.empty
      case NounPhrasePreposition(prepositionalPhrase, nounPhrase) =>
        List(prepositionalPhrase, nounPhrase)
      case ForallQuantifier(nounPhrase) =>
        List(nounPhrase)
      case ExistentialQuantifier(nounPhrase) =>
        List(nounPhrase)
      case DefiniteNounPhrase(nounPhrase) =>
        List(nounPhrase)
      case DemonstrativePronoun(word) =>
        processWord(word)
        List.empty
      case NegativePersonSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case UniversalPersonSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case ExistentialPersonSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case NegativeThingSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case UniversalThingSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case ExistentialThingSingularIndefinitePronoun(word) =>
        processWord(word)
        List.empty
      case FirstPersonSingularPersonalPronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonSingularPersonalPronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonSingularPersonalPronoun(word, gender) =>
        processWord(word)
        processWord(gender.toString)
        List.empty
      case FirstPersonPluralPersonalPronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonPluralPersonalPronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonPluralPersonalPronoun(word) =>
        processWord(word)
        List.empty
      case FirstPersonSingularPossessivePronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonSingularPossessivePronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonSingularPossessivePronoun(word, gender) =>
        processWord(word)
        processWord(gender.toString)
        List.empty
      case FirstPersonPluralPossessivePronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonPluralPossessivePronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonPluralPossessivePronoun(word) =>
        processWord(word)
        List.empty
      case FirstPersonSingularReflexivePronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonSingularReflexivePronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonSingularReflexivePronoun(word, gender) =>
        processWord(word)
        processWord(gender.toString)
        List.empty
      case FirstPersonPluralReflexivePronoun(word) =>
        processWord(word)
        List.empty
      case SecondPersonPluralReflexivePronoun(word) =>
        processWord(word)
        List.empty
      case ThirdPersonPluralReflexivePronoun(word) =>
        processWord(word)
        List.empty
      case AdjectiveNounPhrase(adjective, nounPhrase) =>
        processWord(adjective.word)
        List(nounPhrase)
      case VerbAdverbPhrase(adverb, verbPhrase) =>
        processWord(adverb.word)
        List(verbPhrase)
      case AdjectiveNounPhrase(adjective, nounPhrase) =>
        processWord(adjective.word)
        List(nounPhrase)
    }

    children.foreach(child => {
      val childNode = populate(child, nodes, edges)
      edges += new DagreEdge(newNode.id, childNode.id, newNode.id + "-" + childNode.id)
    })

    newNode
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
    buttonInterpret.onclick = (_: UIEvent) => {
      d3.select("#svg-canvas-stanford").selectAll("*").remove()
      d3.select("#svg-canvas-sensala").selectAll("*").remove()
      loader.style.display = "block"
      termHeading.style.display = "none"
      val request = HttpRequest(s"${window.location.protocol}//${window.location.host}/eval")
        .withMethod(Method.POST)
        .withQueryParameter("discourse", discourseBox.value)
      request.send().onComplete {
        case Success(res) =>
          parse(res.body) match {
            case Right(json) =>
              json.as[List[SensalaInterpretMessage]] match {
                case Right(messages) =>
                  messages.foreach {
                    case StanfordParsed(result) =>
                      Try(renderText(result, "svg-canvas-stanford"))
                    case SensalaParsed(result) =>
                      Try(renderText(result, "svg-canvas-sensala"))
                    case SensalaInterpreted(result) =>
                      termHeading.textContent = result
                      loader.style.display = "none"
                      termHeading.style.display = "block"
                      termHeading.style.color = "black"
                    case SensalaError(error) =>
                      termHeading.textContent = error
                      loader.style.display = "none"
                      termHeading.style.display = "block"
                      termHeading.style.color = "red"
                    case other =>
                      println(s"Other message: $other")
                  }
                case Left(error) =>
                  println(error)
              }
            case Left(failure) =>
              println(failure)
          }
        case Failure(e) =>
          println(e.getMessage)
      }
    }
  }
}
