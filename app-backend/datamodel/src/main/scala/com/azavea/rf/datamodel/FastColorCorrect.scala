package com.azavea.rf.datamodel.fast

import io.circe.generic.JsonCodec

import com.azavea.rf.datamodel._
import com.azavea.rf.datamodel.util._
import com.azavea.rf.datamodel.color._

import geotrellis.raster._
import geotrellis.raster.equalization.HistogramEqualization
import geotrellis.raster.histogram.Histogram

import spire.syntax.cfor._
import org.apache.commons.math4.util.FastMath

import scala.annotation.tailrec

import java.io._

case class Memo[I, K, O](f: I => O)(implicit ev: I => K) extends (I => O) {
  import collection.mutable.{Map => Dict}
  type Input = I
  type Key = K
  type Output = O

  val cache = Dict.empty[K, O]
  override def apply(x: I) = cache.getOrElseUpdate(x, f(x))
}

object WhiteBalance {
  
  @inline def clamp8Bit(z: Int): Int = {
    if (z < 0) 0
    else if (z > 255) 255
    else z
  }

  def apply(rgbTiles: List[MultibandTile]): List[MultibandTile] = {
    val tileAdjs = rgbTiles.par.map(t => tileRgbAdjustments(t)).toList.foldLeft((0.0, 0.0, 0.0))((acc, x) => {
      (acc._1 + (x._1 / rgbTiles.length), acc._2 + (x._2 / rgbTiles.length), acc._3 + (x._3 / rgbTiles.length))
    })

    val newTiles =
      rgbTiles
        .map(t => MultibandTile(
          t.band(0).mapIfSet(c => clamp8Bit((c * tileAdjs._1).toInt)),
          t.band(1).mapIfSet(c => clamp8Bit((c * tileAdjs._2).toInt)),
          t.band(2).mapIfSet(c => clamp8Bit((c * tileAdjs._3).toInt))))
        .map(_.convert(UByteConstantNoDataCellType))

    newTiles
  }

  def tileRgbAdjustments(rgbTile: MultibandTile): (Double, Double, Double) = {
    try {
      val resampledTile = rgbTile.resample(128, 128)
      val newTileAdjs = adjustGrey(resampledTile, (1.0, 1.0, 1.0), 0, false)
      newTileAdjs
    } catch {
      case ex:Exception => {
        val sw = new StringWriter
        ex.printStackTrace(new PrintWriter(sw))
        println(sw.toString)
        throw ex
      }
    }
  }

  def RgbToYuv(rByte: Int, gByte: Int, bByte: Int): YUV = {
    type I = (Int, Int, Int)
    type K = (Int, Int, Int)
    type O = YUV

    type MemoizedFn = Memo[I, K, O]

    implicit def encode(input: MemoizedFn#Input): MemoizedFn#Key = (input._1, input._2, input._3)

    def rgbToYuv(rb: Int, gb: Int, bb: Int): YUV = {
      val (r, g, b) = (rb, gb, bb)
      val W_r = 0.299
      val W_b = 0.114
      val W_g = 1 - W_r - W_b

      val Y = W_r*r + W_g*g + W_b*b
      val U = -0.168736*r + -0.331264*g + 0.5*b
      val V = 0.5*r + -0.418688*g + -0.081312*b

      YUV(Y,U,V)
    }

    lazy val f: MemoizedFn = Memo {
      case (r, g, b) => rgbToYuv(r,g,b)
      case _ => throw new IllegalArgumentException("Wrong number of arguments")
    }

    f((rByte, gByte, bByte))
  }
  
  case class AutoBalanceParams (
    maxIter: Int, gainIncr:Double, doubleStepThreshold:Double, convergenceThreshold:Double, greyThreshold:Double
  )

  case class YUV (y: Double, u: Double, v: Double)

  val balanceParams = AutoBalanceParams(maxIter=1000, gainIncr=0.01, doubleStepThreshold=0.8, convergenceThreshold=0.001, greyThreshold=0.3)

  def mapBands(mbTile:MultibandTile, f: Array[Int] => Option[YUV]): List[List[Option[YUV]]]  = {
    val newTile = MultibandTile(mbTile.bands)
    val array = Array.ofDim[Option[YUV]](newTile.cols, newTile.rows)

    cfor(0)(_ < newTile.rows, _ + 1) { row =>
      cfor(0)(_ < newTile.cols, _ + 1) { col =>
        val bandValues = Array.ofDim[Int](newTile.bandCount)
        cfor(0)(_ < newTile.bandCount, _ + 1) { band =>
          bandValues(band) = newTile.band(band).get(col, row)
        }
        array(col)(row) = f(bandValues)
      }
    }

    array.map(_.toList).toList
  }
  
  @tailrec
  def adjustGrey(rgbTile:MultibandTile, adjustments:(Double, Double, Double), iter:Int, converged:Boolean):(Double, Double, Double) = {
    if (iter>=balanceParams.maxIter || converged) {
      adjustments
    } else {
      // convert tile to YUV
      val tileYuv = mapBands(rgbTile, (rgb) => {
          val bands = rgb.toList.map((i:Int) => if (isData(i)) Some(i) else None)
          val r :: g :: b :: xs = bands
          val rgbs = (r, g, b)
          val yuv = rgbs match {
            case (Some(rd), Some(gr), Some(bl)) =>  Some(RgbToYuv(rd, gr, bl))
            case _ => None
          }
          yuv
      })
      
      // find grey chromaticity
      val offGrey = (yuv:YUV) => (math.abs(yuv.u) + math.abs(yuv.v)) / yuv.y
      val greys = tileYuv.map(lst => lst.map(yuv => {
        for {
          y <- yuv
        } yield {
          if (offGrey(y) < balanceParams.greyThreshold) Some(y) else None
        }
      })).flatten

      // calculate average "off-grey"-ness of U & V
      val uBar = greys.map(yuvs => yuvs.map(mYuv => {
          mYuv match {
            case Some(yuv) => yuv.u
            case _ => 0.0
          }
      })).flatten.sum / greys.length
      
      val vBar = greys.map(yuvs => yuvs.map(mYuv => {
          mYuv match {
            case Some(yuv) => yuv.v
            case _ => 0.0
          }
      })).flatten.sum / greys.length

      // adjust red & blue channels if convergence hasn't been reached
      val err = if (math.abs(uBar) > math.abs(vBar)) uBar else vBar
      val gainVal = (err match {
        case x if x < balanceParams.convergenceThreshold => 0
        case x if x > (balanceParams.doubleStepThreshold * 1) => 2 * balanceParams.gainIncr * math.signum(err)
        case _ => balanceParams.gainIncr * err
      })

      val channelGain = if (math.abs(vBar) > math.abs(uBar)) List((1 - gainVal), 1, 1) else List(1, 1, (1 - gainVal))
      val newAdjustments = (adjustments._1 * channelGain(0), adjustments._2 * channelGain(1), adjustments._3 * channelGain(2))

      val balancedTile = MultibandTile(
        rgbTile.band(0).mapIfSet(c => clamp8Bit((c*channelGain(0)).toInt)),
        rgbTile.band(1).mapIfSet(c => clamp8Bit((c*channelGain(1)).toInt)),
        rgbTile.band(2).mapIfSet(c => clamp8Bit((c*channelGain(2)).toInt)))

      adjustGrey(balancedTile, newAdjustments, iter + 1, if (gainVal == 0) true else false)
    }
  }

}

object ColorCorrect extends TimingLogging {
  import functions.SaturationAdjust._
  import functions.SigmoidalContrast._
  import functions.Approximations

  case class LayerClipping(redMin: Int, redMax: Int, greenMin: Int, greenMax: Int, blueMin: Int, blueMax: Int)
  sealed trait ClipValue
  case class ClipBounds(min: Int, max: Int) extends ClipValue
  case class MaybeClipBounds(maybeMin: Option[Int], maybeMax: Option[Int]) extends ClipValue
  case class ClippingParams(band: Int, bounds: ClipValue)

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
    def getGamma: Map[Int, Option[Double]] = Map(0 -> gamma.redGamma, 1 -> gamma.greenGamma, 2 -> gamma.blueGamma)

    def reorderBands(tile: MultibandTile, hist: Seq[Histogram[Double]]): (MultibandTile, Array[Histogram[Double]]) =
      (tile.subsetBands(redBand, greenBand, blueBand), Array(hist(redBand), hist(greenBand), hist(blueBand)))

    def colorCorrect(tile: MultibandTile, hist: Seq[Histogram[Double]]): MultibandTile = {
      val (rgbTile, rgbHist) = reorderBands(tile, hist)
      ColorCorrect(rgbTile, rgbHist, this)
    }
  }

  @inline def normalizeAndClampAndGammaCorrectPerPixel(z: Int, oldMin: Int, oldMax: Int, newMin: Int, newMax: Int, gammaOpt: Option[Double]): Int = {
    if(isData(z)) {
      val dNew = newMax - newMin
      val dOld = oldMax - oldMin

      // When dOld is nothing (normalization is meaningless in this context), we still need to clamp
      if (dOld == 0) {
        val v = {
          if (z > newMax) newMax
          else if (z < newMin) newMin
          else z
        }

        gammaOpt match {
          case None => v
          case Some(gamma) => {
            clampColor {
              val gammaCorrection = 1 / gamma
              (255 * FastMath.pow(v / 255.0, gammaCorrection)).toInt
            }
          }
        }
      } else {
        val v = {
          val scaled = (((z - oldMin) * dNew) / dOld) + newMin

          if (scaled > newMax) newMax
          else if (scaled < newMin) newMin
          else scaled
        }

        gammaOpt match {
          case None => v
          case Some(gamma) => {
            clampColor {
              val gammaCorrection = 1 / gamma
              (255 * FastMath.pow(v / 255.0, gammaCorrection)).toInt
            }
          }
        }
      }
    } else z
  }

  val rgbBand =
    (specificBand: Option[Int], allBands: Option[Int], tileDefault: Int) =>
      specificBand.fold(allBands)(Some(_)).fold(Some(tileDefault))(x => Some(x))

  def complexColorCorrect(rgbTile: MultibandTile, chromaFactor: Option[Double])
                         (layerNormalizeArgs: Map[Int, ClipBounds], gammas: Map[Int, Option[Double]])
                         (sigmoidalContrast: SigmoidalContrast)
                         (colorCorrectArgs: Map[Int, MaybeClipBounds], tileClipping: MultiBandClipping): MultibandTile = {
    val (red, green, blue) = (rgbTile.band(0), rgbTile.band(1), rgbTile.band(2))
    val (gr, gg, gb) = (gammas(0), gammas(1), gammas(2))
    val (nred, ngreen, nblue) = (
      ArrayTile.alloc(rgbTile.cellType, rgbTile.cols, rgbTile.rows),
      ArrayTile.alloc(rgbTile.cellType, rgbTile.cols, rgbTile.rows),
      ArrayTile.alloc(rgbTile.cellType, rgbTile.cols, rgbTile.rows)
    )

    val ClipBounds(rmin, rmax) = layerNormalizeArgs(0)
    val ClipBounds(gmin, gmax) = layerNormalizeArgs(1)
    val ClipBounds(bmin, bmax) = layerNormalizeArgs(2)

    val (rclipMin, rclipMax, rnewMin, rnewMax) = (rmin, rmax, 0, 255)
    val (gclipMin, gclipMax, gnewMin, gnewMax) = (gmin, gmax, 0, 255)
    val (bclipMin, bclipMax, bnewMin, bnewMax) = (bmin, bmax, 0, 255)

    val sigmoidal: Double => Double =
      (sigmoidalContrast.alpha, sigmoidalContrast.beta) match {
        case (Some(a), Some(b)) => localTransform(rgbTile.cellType, a, b)
        case _ => identity
      }

    val (clipr, clipg, clipb): (Int => Int, Int => Int, Int => Int) = {
      val MaybeClipBounds(mrmin, mrmax) = colorCorrectArgs(0)
      val MaybeClipBounds(mgmin, mgmax) = colorCorrectArgs(1)
      val MaybeClipBounds(mbmin, mbmax) = colorCorrectArgs(2)

      val (mrclipMin, mrclipMax) = (rgbBand(mrmin, tileClipping.min, 0).get, rgbBand(mrmax, tileClipping.max, 255).get)
      val (mgclipMin, mgclipMax) = (rgbBand(mgmin, tileClipping.min, 0).get, rgbBand(mgmax, tileClipping.max, 255).get)
      val (mbclipMin, mbclipMax) = (rgbBand(mbmin, tileClipping.min, 0).get, rgbBand(mbmax, tileClipping.max, 255).get)

      @inline def clipBands(z: Int, min: Int, max: Int): Int = {
        if (isData(z) && z > max) 255
        else if (isData(z) && z < min) 0
        else z
      }

      (clipBands(_, mrclipMin, mrclipMax), clipBands(_, mgclipMin, mgclipMax), clipBands(_, mbclipMin, mbclipMax))
    }

    timedCreate("SaturationAdjust", "190::cfor start", "190::cfor finish") {
      cfor(0)(_ < rgbTile.cols, _ + 1) { col =>
        cfor(0)(_ < rgbTile.rows, _ + 1) { row =>
          val (r, g, b) =
            (ColorCorrect.normalizeAndClampAndGammaCorrectPerPixel(red.get(col, row), rclipMin, rclipMax, rnewMin, rnewMax, gr),
              ColorCorrect.normalizeAndClampAndGammaCorrectPerPixel(green.get(col, row), gclipMin, gclipMax, gnewMin, gnewMax, gg),
              ColorCorrect.normalizeAndClampAndGammaCorrectPerPixel(blue.get(col, row), bclipMin, bclipMax, bnewMin, bnewMax, gb))

          val (nr, ng, nb) = chromaFactor match {
            case Some(cf) => {
              val (hue, chroma, luma) = RGBToHCLuma(r, g, b)
              val newChroma = scaleChroma(chroma, cf)
              val (nr, ng, nb) = HCLumaToRGB(hue, newChroma, luma)
              (nr, ng, nb)
            }

            case _ => (r, g, b)
          }

          nred.set(col, row, clipr(sigmoidal(nr).toInt))
          ngreen.set(col, row, clipg(sigmoidal(ng).toInt))
          nblue.set(col, row, clipb(sigmoidal(nb).toInt))
        }
      }
    }

    printBuffer("SaturationAdjust")
    MultibandTile(nred, ngreen, nblue)
  }

  def apply(rgbTile: MultibandTile, rgbHist: Array[Histogram[Double]], params: Params): MultibandTile = {
    var _rgbTile = rgbTile
    val gammas = params.getGamma
    if (params.equalize.enabled) _rgbTile = timedCreate("FastColorCorrect", "538::HistogramEqualization start", "538::HistogramEqualization finish") {
      HistogramEqualization(rgbTile, rgbHist)
    }

    val layerRgbClipping = timedCreate("FastColorCorrect", "542::layerRgbClipping start", "538::layerRgbClipping finish") {
      val range = 1 until 255
      var isCorrected = true
      val iMaxMin: Array[(Int, Int)] = Array.ofDim(3)
      cfor(0)(_ < rgbHist.length, _ + 1) { index =>
        val hst = rgbHist(index)
        val imin = hst.minValue().map(_.toInt).getOrElse(0)
        val imax = hst.maxValue().map(_.toInt).getOrElse(255)
        iMaxMin(index) = (imin, imax)
        isCorrected &&= {
          if (range.contains(hst.minValue().map(_.toInt).getOrElse(0))) true
          else if (range.contains(hst.maxValue().map(_.toInt).getOrElse(255))) true
          else false
        }
      }

      if (!isCorrected) {
        val (rmin, rmax) = iMaxMin(0)
        val (gmin, gmax) = iMaxMin(1)
        val (bmin, bmax) = iMaxMin(2)
        LayerClipping(rmin, rmax, gmin, gmax, bmin, bmax)
      } else LayerClipping(0, 255, 0, 255, 0, 255)
    }

    val layerNormalizeArgs: Map[Int, ClipBounds] = Map(
      0 -> ClipBounds(layerRgbClipping.redMin, layerRgbClipping.redMax),
      1 -> ClipBounds(layerRgbClipping.greenMin, layerRgbClipping.greenMax),
      2 -> ClipBounds(layerRgbClipping.blueMin, layerRgbClipping.blueMax)
    )

    val colorCorrectArgs: Map[Int, MaybeClipBounds] = Map(
      0 -> MaybeClipBounds(params.bandClipping.redMin, params.bandClipping.redMax),
      1 -> MaybeClipBounds(params.bandClipping.greenMin, params.bandClipping.greenMax),
      2 -> MaybeClipBounds(params.bandClipping.blueMin, params.bandClipping.blueMax)
    )


    _rgbTile = timedCreate("FastColorCorrect", "579::SaturationAdjust.complex start", "579::SaturationAdjust.complex finish") {
      complexColorCorrect(_rgbTile, params.saturation.saturation)(layerNormalizeArgs, gammas)(params.sigmoidalContrast)(colorCorrectArgs, params.tileClipping)
    }

    printBuffer("FastColorCorrect")

    _rgbTile
  }


  @inline def clampColor(z: Int): Int = {
    if (z < 0) 0
    else if (z > 255) 255
    else z
  }

  def maxCellValue(ct: CellType): Int =
    ct match {
      case _: FloatCells =>
        Float.MaxValue.toInt
      case _: DoubleCells =>
        Double.MaxValue.toInt
      case _: BitCells | _: UByteCells | _: UShortCells =>
        (1 << ct.bits) - 1
      case _: ByteCells | _: ShortCells | _: IntCells =>
        ((1 << ct.bits) - 1) - (1 << (ct.bits - 1))
    }

  def clipBands(tile: Tile, min: Int, max: Int): Tile = {
    tile.mapIfSet { z =>
      if (z > max) 255
      else if (z < min) 0
      else z
    }
  }

  def normalizeAndClamp(tile: Tile, oldMin: Int, oldMax: Int, newMin: Int, newMax: Int): Tile = {
    val dNew = newMax - newMin
    val dOld = oldMax - oldMin

    // When dOld is nothing (normalization is meaningless in this context), we still need to clamp
    if (dOld == 0) tile.mapIfSet { z =>
      if (z > newMax) newMax
      else if (z < newMin) newMin
      else z
    } else tile.mapIfSet { z =>
      val scaled = (((z - oldMin) * dNew) / dOld) + newMin

      if (scaled > newMax) newMax
      else if (scaled < newMin) newMin
      else scaled
    }
  }

  def gammaCorrect(tile: Tile, gamma: Double): Tile =
    tile.mapIfSet { z =>
      clampColor {
        val gammaCorrection = 1 / gamma
        (255 * FastMath.pow(z / 255.0, gammaCorrection)).toInt
      }
    }
}
