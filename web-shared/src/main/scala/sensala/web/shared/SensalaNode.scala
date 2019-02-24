package sensala.web.shared

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads
import play.api.libs.json.Writes

final case class SensalaNode(label: String, nodeType: String, children: List[SensalaNode])

object SensalaNode {
  val stanfordNodeReads: Reads[SensalaNode] = (
    (JsPath \ "label").read[String] and
      (JsPath \ "nodeType").read[String] and
      (JsPath \ "children").lazyRead(Reads.list[SensalaNode](stanfordNodeReads))
  )(SensalaNode.apply _)

  val stanfordNodeWrites: Writes[SensalaNode] = (
    (JsPath \ "label").write[String] and
      (JsPath \ "nodeType").write[String] and
      (JsPath \ "children").lazyWrite(Writes.list[SensalaNode](stanfordNodeWrites))
  )(unlift(SensalaNode.unapply))

  implicit val jsonFormat = Format(stanfordNodeReads, stanfordNodeWrites)
}
