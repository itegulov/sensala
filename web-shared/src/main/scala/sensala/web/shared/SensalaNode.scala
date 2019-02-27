package sensala.web.shared

import io.circe._
import io.circe.generic.semiauto._

final case class SensalaNode(label: String, nodeType: String, children: List[SensalaNode])

object SensalaNode {
  implicit val decoder: Decoder[SensalaNode] = deriveDecoder[SensalaNode]
  implicit val encoder: Encoder[SensalaNode] = deriveEncoder[SensalaNode]
}
