package com.azavea.rf.ingest.model

import spray.json._
import DefaultJsonProtocol._

import java.util.UUID

/** An ingest layer groups together input sources which should be within the same catalog
  *
  * @param id A UUID for this particular layer (used later in providing a key to the GT catalog)
  * @param output An [[OutputDefinition]] which specifies how to save a set of tiles
  * @param sources A list of source specifications
  */
case class IngestLayer(
  id: UUID,
  output: OutputDefinition,
  sourceOverrides: Array[SourceDefinition.Overrides]
) {
  lazy val sources: Array[SourceDefinition] = sourceOverrides.map(SourceDefinition(_))
}

object IngestLayer {
  implicit object ingestLayerJsonFormat extends RootJsonFormat[IngestLayer] {
      def write(obj: IngestLayer) = JsObject(
        "id" -> obj.id.toJson,
        "output" -> obj.output.toJson,
        "sourceOverrides" -> obj.sourceOverrides.toJson
      )
      def read(json: JsValue) = json.asJsObject.getFields("id", "output", "sourceOverrides") match {
        case Seq(i, o, s) =>
          IngestLayer(i.convertTo[UUID], o.convertTo[OutputDefinition], s.convertTo[Array[SourceDefinition.Overrides]])
        case _ =>
          deserializationError("Failed to parse IngestLayer")
      }

  }

}
