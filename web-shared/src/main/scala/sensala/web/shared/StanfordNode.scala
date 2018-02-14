package sensala.web.shared

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads
import play.api.libs.json.Writes

final case class StanfordNode(label: String, nodeType: String, children: List[StanfordNode])

object StanfordNode {
  val stanfordNodeReads: Reads[StanfordNode] = (
      (JsPath \ "label").read[String] and
      (JsPath \ "nodeType").read[String] and
      (JsPath \ "children").lazyRead(Reads.list[StanfordNode](stanfordNodeReads))
    )(StanfordNode.apply _)

  val stanfordNodeWrites: Writes[StanfordNode] = (
      (JsPath \ "label").write[String] and
      (JsPath \ "nodeType").write[String] and
      (JsPath \ "children").lazyWrite(Writes.list[StanfordNode](stanfordNodeWrites))
    )(unlift(StanfordNode.unapply))
  
  implicit val jsonFormat = Format(stanfordNodeReads, stanfordNodeWrites)
}
