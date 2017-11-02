package sensala.conceptnet.structure

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import sensala.conceptnet.structure.auxilary._

package object json {
  implicit val idReads: Reads[ConceptNetId] = JsPath.read[String].map(ConceptNetId.apply)
  
  implicit val languageReads: Reads[ConceptNetLanguage] = JsPath.read[String].map {
    case "en" => English
    case _ => OtherLanguage
  }
  
  implicit val relationReads: Reads[ConceptNetRelation] = (JsPath \ "label").read[String].map {
    case "RelatedTo" => RelatedTo
    case "Synonym" => Synonym
    case "Antonym" => Antonym
    case "CapableOf" => CapableOf
    case "IsA" => IsA
    case "ExternalURL" => ExternalUrl
    case "FormOf" => FormOf
    case "HasContext" => HasContext
    case "PartOf" => PartOf
    case "SimilarTo" => SimilarTo
    case "UsedFor" => UsedFor
    case "genus" => Genus
    case "EtymologicallyRelatedTo" => EtymologicallyRelatedTo
    case "AtLocation" => AtLocation
    case "Desires" => Desires
    case _ => OtherRelation
  }
  
  implicit val sourceReads: Reads[ConceptNetSource] = (
    (JsPath \ "@id").read[ConceptNetId] and
    (JsPath \ "contributor").read[String] and
    (JsPath \ "process").readNullable[String]
  )(ConceptNetSource.apply _)
  
  implicit val endpointReads: Reads[ConceptNetEndpoint] = (
    (JsPath \ "@id").read[ConceptNetId] and
    (JsPath \ "label").read[String] and
    (JsPath \ "language").readWithDefault[ConceptNetLanguage](English) and
    (JsPath \ "term").read[String]
  )(ConceptNetEndpoint.apply _)
  
  implicit val edgeReads: Reads[ConceptNetEdge] = (
    (JsPath \ "@id").read[ConceptNetId] and
    (JsPath \ "start").read[ConceptNetEndpoint] and
    (JsPath \ "end").read[ConceptNetEndpoint] and
    (JsPath \ "dataset").read[String] and
    (JsPath \ "license").read[String] and
    (JsPath \ "rel").read[ConceptNetRelation] and
    (JsPath \ "sources").read[List[ConceptNetSource]] and
    (JsPath \ "surfaceText").readNullable[String] and
    (JsPath \ "weight").read[Double]
  )(ConceptNetEdge.apply _)
  
  implicit val viewReads: Reads[ConceptNetView] = (
    (JsPath \ "@id").read[ConceptNetId] and
    (JsPath \ "firstPage").read[String] and
    (JsPath \ "nextPage").readNullable[String] and
    (JsPath \ "previousPage").readNullable[String] and
    (JsPath \ "paginatedProperty").read[String]
  )(ConceptNetView.apply _)
  
  implicit val wordPageReads: Reads[ConceptNetWordPage] = (
    (JsPath \ "@id").read[ConceptNetId] and
    (JsPath \ "@context").read[List[String]] and
    (JsPath \ "edges").read[List[ConceptNetEdge]] and
    (JsPath \ "view").readNullable[ConceptNetView]
  )(ConceptNetWordPage.apply _)
}
