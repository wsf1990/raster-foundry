package com.azavea.rf.ingest.model

import java.net.URI
import spray.json._
import DefaultJsonProtocol._

import geotrellis.raster._
import geotrellis.vector._
import geotrellis.proj4.CRS

import com.azavea.rf.ingest.util.getTiffTags

/** This class provides all information required to read from a source
  *
  * @param uri      The URI of the source imagery
  * @param extent   The Extent of a source tile
  * @param crs      The CRS of the projection to target in tiling
  * @param bandMaps A list of mappings from source to destination tile
  */
case class SourceDefinition(
  uri: URI,
  extent: Extent,
  extentCrs: CRS,
  crs: CRS,
  cellSize: CellSize,
  bandMaps: Array[BandMapping]
)

object SourceDefinition {
  implicit val jsonFormat = jsonFormat6(SourceDefinition.apply _)

  def apply(overrides: SourceDefinition.Overrides): SourceDefinition = {
    lazy val tt = getTiffTags(overrides.uri)
    lazy val extent = overrides.extent.getOrElse(tt.extent)
    lazy val crs = overrides.crs.getOrElse(tt.crs)
    lazy val extentCrs = overrides.extentCrs.getOrElse(crs)
    lazy val cellSize = overrides.cellSize.getOrElse(tt.cellSize)

    SourceDefinition(
      overrides.uri,
      extent,
      crs,
      extentCrs,
      cellSize,
      overrides.bandMaps
    )
  }

  /** This object handles deserialization of source definitions and overrides any values which
    *  might be found in the tiff's header.
    */
  case class Overrides(
    uri: URI,
    extent: Option[Extent],
    extentCrs: Option[CRS],
    crs: Option[CRS],
    cellSize: Option[CellSize],
    bandMaps: Array[BandMapping]
  )

  object Overrides {
    implicit val jsonFormat = jsonFormat6(Overrides.apply _)
  }
}
