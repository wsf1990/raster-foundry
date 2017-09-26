package com.azavea.rf

import com.azavea.rf.bridge._

import spray.json._
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.semiauto._
import cats.syntax.either._
import geotrellis.proj4._
import geotrellis.slick._
import geotrellis.vector._
import geotrellis.vector.io._
import geotrellis.raster.io._
import geotrellis.raster.render._
import geotrellis.raster.histogram._
import DefaultJsonProtocol._

import scala.util._
import java.net.URI
import java.sql.Timestamp
import java.time.Instant
import java.util.UUID
import java.security.InvalidParameterException


package object datamodel {

  trait OwnerCheck {
    def checkOwner(createUser: User, ownerUserId: Option[String]): String = {
      (createUser, ownerUserId) match {
        case (user, Some(id)) if user.id == id => user.id
        case (user, Some(id)) if user.isInRootOrganization => id
        case (user, Some(id)) if !user.isInRootOrganization =>
          throw new IllegalArgumentException("Insufficient permissions to set owner on object")
        case (user, _) => user.id
      }
    }
  }

  implicit def encodePaginated[A: Encoder] =
    Encoder.forProduct6(
      "count",
      "hasPrevious",
      "hasNext",
      "page",
      "pageSize",
      "results"
    )({
      pr: PaginatedResponse[A] =>
      (pr.count, pr.hasPrevious, pr.hasNext, pr.page, pr.pageSize, pr.results)
    }
    )

  implicit val timestampEncoder: Encoder[Timestamp] =
    Encoder.encodeString.contramap[Timestamp](_.toInstant.toString)
  implicit val timestampDecoder: Decoder[Timestamp] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(Timestamp.from(Instant.parse(str))).leftMap(_ => "Timestamp")
    }

  implicit val uuidEncoder: Encoder[UUID] =
    Encoder.encodeString.contramap[UUID](_.toString)
  implicit val uuidDecoder: Decoder[UUID] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(UUID.fromString(str)).leftMap(_ => "UUID")
    }

  implicit val uriEncoder: Encoder[URI] =
    Encoder.encodeString.contramap[URI] { _.toString }
  implicit val uriDecoder: Decoder[URI] =
    Decoder.decodeString.emap { str =>
      Either.catchNonFatal(URI.create(str)).leftMap(_ => "URI")
    }

  implicit val projectedGeometryEncoder: Encoder[Projected[Geometry]] =
    new Encoder[Projected[Geometry]] {
      final def apply(g: Projected[Geometry]): Json = {
        val reprojected = g match {
          case Projected(geom, 4326) => geom
          case Projected(geom, 3857) => geom.reproject(WebMercator, LatLng)
          case Projected(geom, srid) => try {
            geom.reproject(CRS.fromString(s"EPSG:$srid"), LatLng)
          } catch {
            case e: Exception =>
              throw new InvalidParameterException(s"Unsupported Geometry SRID: $srid").initCause(e)
          }
        }
        parse(reprojected.toGeoJson) match {
          case Right(js: Json) => js
          case Left(e) => throw e
        }
      }
    }

  implicit val projectedMultiPolygonEncoder: Encoder[Projected[MultiPolygon]] =
    new Encoder[Projected[MultiPolygon]] {
      final def apply(g: Projected[MultiPolygon]): Json = {
        val reprojected = g match {
          case Projected(geom, 4326) => geom
          case Projected(geom, 3857) => geom.reproject(WebMercator, LatLng)
          case Projected(geom, srid) => try {
            geom.reproject(CRS.fromString(s"EPSG:$srid"), LatLng)
          } catch {
            case e: Exception =>
              throw new InvalidParameterException(s"Unsupported MultiPolygon SRID: $srid").initCause(e)
          }
        }
        parse(reprojected.toGeoJson) match {
          case Right(js: Json) => js
          case Left(e) => throw e
        }
      }
    }


  // TODO: make this tolerate more than one incoming srid
  implicit val projectedGeometryDecoder: Decoder[Projected[Geometry]] = Decoder[Json] map { js =>
    Projected(
      js.spaces4.parseGeoJson[Geometry], 4326
    ).reproject(
      CRS.fromEpsgCode(4326), CRS.fromEpsgCode(3857)
    )(3857)
  }

  implicit val projectedMultiPolygonDecoder: Decoder[Projected[MultiPolygon]] = Decoder[Json] map { js =>
    Projected(
      js.spaces4.parseGeoJson[MultiPolygon], 4326
    ).reproject(
      CRS.fromEpsgCode(4326), CRS.fromEpsgCode(3857)
    )(3857)
  }

  // RGBA deserialization
  implicit val decodeHexRGBA: Decoder[RGBA] = Decoder.decodeString.emap { str =>
    str.stripPrefix("#").stripPrefix("0x") match {
      case hex if (hex.size == 8) =>
        val bytes = hex.sliding(2, 2).map({ hexByte => Integer.parseInt(hexByte, 16) }).toList
        Right(RGBA(bytes(0), bytes(1), bytes(2), bytes(3)))
      case hex if (hex.size == 6) =>
        val bytes = hex.sliding(2, 2).map({ hexByte => Integer.parseInt(hexByte, 16) }).toList
        Right(RGB(bytes(0), bytes(1), bytes(2)))
      case hex => Left(s"Unable to parse $hex as an RGBA")
    }
  }
  implicit val encodeRgbaAsHex: Encoder[RGBA] = Encoder.encodeString.contramap[RGBA] { rgba =>
    "#" + rgba.red.toHexString + rgba.blue.toHexString + rgba.green.toHexString + rgba.alpha.toHexString
  }

  implicit val histogramDecoder: Decoder[Histogram[Double]] = Decoder[Json].map { js =>
    js.noSpaces.parseJson.convertTo[Histogram[Double]]
  }
  implicit val histogramEncoder: Encoder[Histogram[Double]] = new Encoder[Histogram[Double]] {
    final def apply(hist: Histogram[Double]): Json = hist.toJson.asJson
  }

  implicit val colorRampDecoder: Decoder[ColorRamp] =
    Decoder[Vector[Int]].map({ ColorRamp(_) })
  implicit val colorRampEncoder: Encoder[ColorRamp] = new Encoder[ColorRamp] {
    final def apply(cRamp: ColorRamp): Json = cRamp.colors.toArray.asJson
  }
}
