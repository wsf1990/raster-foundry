package com.azavea.rf.datamodel

import com.azavea.rf.bridge._

import io.circe._
import io.circe.parser._
import io.circe.syntax._
import io.circe.generic.semiauto._
import cats.syntax.either._
import spire.std.any._
import geotrellis.raster._
import geotrellis.raster.render._

import scala.util.{Try, Success, Failure}
import java.security.InvalidParameterException


case class ClassMap(
  classifications: Map[Double, Int]
) {
  // How exposed should this be to the api?
  val options: ClassMap.Options = ClassMap.Options()

  lazy val mapStrategy =
    new MapStrategy(options.boundaryType, options.ndValue, options.fallback, false)

  def toBreakMap =
    new BreakMap(classifications, mapStrategy, { i: Double => isNoData(i) })

  def toColorMap =
    ColorMap(
      classifications,
      ColorMap.Options(
        options.boundaryType,
        options.ndValue,
        options.fallback
      )
    )
}

object ClassMap {
  case class Options(
    boundaryType: ClassBoundaryType = LessThanOrEqualTo,
    ndValue: Int = NODATA,
    fallback: Int = NODATA
  )

  val defaultClassMapDecoder: Decoder[ClassMap] = deriveDecoder[ClassMap]
  val hexClassMapDecoder: Decoder[ClassMap] = new Decoder[ClassMap] {
    final def apply(c: HCursor): Decoder.Result[ClassMap] = {
      Try {
      for {
        hexes <- c.downField("classifications").as[Map[Double, String]]
      } yield {
        val updated: Map[Double, Int] = hexes.mapValues({ hex =>
          hex match {
            case hex if (hex.size == 8) =>
              val bytes = hex.sliding(2, 2).map({ hexByte => Integer.parseInt(hexByte, 16) }).toList
              RGBA(bytes(0), bytes(1), bytes(2), bytes(3))
            case hex if (hex.size == 6) =>
              val bytes = hex.sliding(2, 2).map({ hexByte => Integer.parseInt(hexByte, 16) }).toList
              RGB(bytes(0), bytes(1), bytes(2))
            case unrecognized =>
              throw new InvalidParameterException(s"'$unrecognized' is not a recognized ClassBoundaryType")
          }
        })
        updated
      }
    } match {
      case Success(update) => update.map({ new ClassMap(_) })
      case Failure(e) => Left(DecodingFailure(s"Unable to parse classmap: ${e.getMessage}", c.history))
    }
    }
  }

  implicit val classMapDecoder: Decoder[ClassMap] = defaultClassMapDecoder or hexClassMapDecoder
  implicit val classMapEncoder: Encoder[ClassMap] = deriveEncoder[ClassMap]
}

