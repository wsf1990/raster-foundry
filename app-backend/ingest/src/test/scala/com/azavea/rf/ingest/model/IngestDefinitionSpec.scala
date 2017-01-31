package com.azavea.rf.ingest.model

import com.azavea.rf.ingest.model._

import geotrellis.raster._
import spray.json._
import DefaultJsonProtocol._
import org.scalatest._

import scala.io.Source


class IngestDefinitionSpec extends FunSpec with Matchers {

  it("parses the sample, local definition") {
    noException should be thrownBy {
      Source
        .fromURL(getClass.getResource("/localjob.json"))
        .getLines
        .mkString("\n")
        .parseJson
        .convertTo[IngestDefinition]
    }
  }

  it("parses the sample, aws definition") {
    noException should be thrownBy {
      Source
        .fromURL(getClass.getResource("/localjob.json"))
        .getLines
        .mkString("\n")
        .parseJson
        .convertTo[IngestDefinition]
    }
  }
}
