package com.azavea.rf.tile

import com.azavea.rf.tool._
import com.azavea.rf.datamodel._

import geotrellis.raster.Tile
import geotrellis.raster._
import geotrellis.raster.render._
import geotrellis.raster.render.png._
import geotrellis.raster.histogram.Histogram

import scala.math.abs
import java.util.Arrays.binarySearch


package object image {
  implicit class AlternativeTileRenderingMethods(tile: Tile) extends RFMLRendering
}
