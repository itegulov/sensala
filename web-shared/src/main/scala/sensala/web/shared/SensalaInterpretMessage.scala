package sensala.web.shared

import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait SensalaInterpretMessage

object SensalaInterpretMessage {
  implicit val jsonFormat: OFormat[SensalaInterpretMessage] = derived.oformat[SensalaInterpretMessage]()
}

final case class SensalaRunInterpretation(discourse: String) extends SensalaInterpretMessage

final case class SensalaParsed(result: String) extends SensalaInterpretMessage

final case class SensalaInterpreted(result: String) extends SensalaInterpretMessage

case object KeepAliveMsg extends SensalaInterpretMessage

// Errors
final case class SensalaError(error: String) extends SensalaInterpretMessage
