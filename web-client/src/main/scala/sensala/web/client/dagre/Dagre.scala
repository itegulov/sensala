package sensala.web.client.dagre

import scala.scalajs.js

object Dagre {

  private def fun0[T0](x: js.Function0[T0]): js.Function0[T0] = x

  def newD3Digraph =
    new `dagreD3.graphlib.Graph`(js.Dynamic.literal())
      .setGraph(js.Dynamic.literal())
      .setDefaultEdgeLabel(fun0 { () =>
        js.Dynamic.literal()
      })

  def newD3Digraph(gOpt: js.Object, fEdgeLabel: js.Function0[js.Dynamic]) =
    new `dagreD3.graphlib.Graph`(gOpt)
      .setGraph(js.Dynamic.literal())
      .setDefaultEdgeLabel(fEdgeLabel)

  def newD3Renderer = new `dagreD3.render`
}

@js.native
trait Graph extends js.Object

@js.native
class `dagreD3.graphlib.Graph`(opt: js.Object) extends Graph {

  def setGraph(o: js.Dynamic): `dagreD3.graphlib.Graph` = js.native

  def setDefaultEdgeLabel(f: js.Function0[js.Dynamic]): `dagreD3.graphlib.Graph` = js.native

  def setNode(id: String, props: js.Any): js.Any = js.native

  def setEdge(sourceId: String, targetId: String, attr: js.Any = null): js.Any = js.native

  def graph(): `dagreD3.graphlib.Graph` = js.native

  def width(): Double = js.native

  def height(): Double = js.native
}

@js.native
class `dagreD3.render` extends js.Object {
  def apply(target: js.Any, g: Graph): js.Any = js.native
}
