package com.azavea.rf.datamodel.stac

import com.azavea.rf.bridge._
import geotrellis.vector.Geometry
import geotrellis.slick.Projected

import io.circe._
import io.circe.generic.JsonCodec

@JsonCodec
case class Feature(
  id: String,
  bbox: Option[List[Double]] = None,
  geometry: Option[Projected[Geometry]] = None,
  assets: Seq[Asset],
  properties: Properties
)

object Feature {}
