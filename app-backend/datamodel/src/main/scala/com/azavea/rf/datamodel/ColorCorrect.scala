package com.azavea.rf.datamodel.deprecated

import java.io._

import io.circe._
import io.circe.syntax._
import io.circe.generic.JsonCodec
import spire.syntax.cfor._
import geotrellis.raster.{MultibandTile, _}
import geotrellis.raster.equalization.HistogramEqualization
import geotrellis.raster.histogram.Histogram
import geotrellis.raster.sigmoidal.SigmoidalContrast
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._

import scala.math._
import scala.annotation.tailrec
import com.azavea.rf.datamodel.color._
import com.azavea.rf.datamodel.util.TimingLogging
import com.azavea.rf.tool.eval.LazyTile
import geotrellis.raster._
import geotrellis.raster.histogram._

object ColorCorrect extends TimingLogging {

  // TODO: Now that each correction is a separate class, it should be possible to refactor this object to place the
  // necessary corrections with the classes that enable them. So rather than
  // ```
  // val maybeAdjustContrast =
  //   for (c <- params.brightContrast.contrast)
  //     yield (mb: MultibandTile) => mb.mapBands { (i, tile) => adjustContrast(tile, c) }
  // ```
  //
  // We would do something like `params.brightContrast.adjustContrast(tile)`
  @JsonCodec
  case class Params(
    redBand: Int, greenBand: Int, blueBand: Int,
    gamma: BandGamma,
    bandClipping: PerBandClipping,
    tileClipping: MultiBandClipping,
    sigmoidalContrast: SigmoidalContrast,
    saturation: Saturation,
    equalize: Equalization,
    autoBalance: AutoWhiteBalance
  ) {
    def reorderBands(tile: MultibandTile, hist: Seq[Histogram[Double]]): (MultibandTile, Array[Histogram[Double]]) =
      (tile.subsetBands(redBand, greenBand, blueBand), Array(hist(redBand), hist(greenBand), hist(blueBand)))

    /*def colorCorrect(tile: MultibandTile, hist: Seq[Histogram[Double]]): MultibandTile = {
      val (rgbTile, rgbHist) = timedCreate("Params", "314::reorderBands start", "314::reorderBands finish") { reorderBands(tile, hist) }
      val result = timedCreate("Params", "315::ColorCorrect start", "315::ColorCorrect finish") {
        fast.ColorCorrect(rgbTile, rgbHist, fast.ColorCorrect.Params(
          redBand, greenBand, blueBand,
          gamma, bandClipping, tileClipping,
          sigmoidalContrast, saturation, equalize,
          autoBalance
        ))
      }
      printBuffer("Params")
      result
    }*/
  }
}
