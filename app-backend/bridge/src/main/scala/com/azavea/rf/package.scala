package com.azavea.rf

import geotrellis.proj4.CRS
import geotrellis.vector.{Extent, MultiPolygon}
import geotrellis.vector.io._
import cats.syntax.either._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import spray.json._

import scala.util.Try


/** Orphaned typeclass instances for GeoTrellis types, as needed by Raster
  * Foundry. This is chiefly for Circe codecs, which GeoTrellis itself doesn't
  * support yet.
  */
package object bridge {

  implicit val sprayJsonEncoder: Encoder[JsValue] = new Encoder[JsValue] {
    final def apply(jsvalue: JsValue): Json = parse(jsvalue.compactPrint) match {
      case Right(success) => success
      case Left(fail) => throw fail
    }
  }

  // Double key (de)serialization
  implicit val decodeKeyDouble: KeyDecoder[Double] = new KeyDecoder[Double] {
    final def apply(key: String): Option[Double] = Try(key.toDouble).toOption
  }
  implicit val encodeKeyDouble: KeyEncoder[Double] = new KeyEncoder[Double] {
    final def apply(key: Double): String = key.toString
  }

  implicit val crsEncoder: Encoder[CRS] =
    Encoder.encodeString.contramap[CRS] { crs => crs.epsgCode.map { c => s"epsg:$c" }.getOrElse(crs.toProj4String) }

  implicit val crsDecoder: Decoder[CRS] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(Try(CRS.fromName(str)) getOrElse CRS.fromString(str)).leftMap(_ => "CRS")
    }

  implicit val extentEncoder: Encoder[Extent] =
    new Encoder[Extent] {
      final def apply(extent: Extent): Json =
        List(extent.xmin, extent.ymin, extent.xmax, extent.ymax).asJson
    }
  implicit val extentDecoder: Decoder[Extent] =
    Decoder[Json] emap { js =>
      js.as[List[Double]].map { case List(xmin, ymin, xmax, ymax) =>
        Extent(xmin, ymin, xmax, ymax)
      }.leftMap(_ => "Extent")
    }

  implicit val multipolygonEncoder: Encoder[MultiPolygon] =
    new Encoder[MultiPolygon] {
      final def apply(mp: MultiPolygon): Json = {
        parse(mp.toGeoJson) match {
          case Right(js: Json) => js
          case Left(e) => throw e
        }
      }
    }

  implicit val multipolygonDecoder: Decoder[MultiPolygon] = Decoder[Json] map {
    _.spaces4.parseGeoJson[MultiPolygon]
  }
}
