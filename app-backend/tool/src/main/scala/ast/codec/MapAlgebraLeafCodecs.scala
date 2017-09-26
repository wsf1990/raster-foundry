package com.azavea.rf.tool.ast.codec

import com.azavea.rf.tool.ast._

import io.circe._
import io.circe.syntax._
import geotrellis.raster._

import java.security.InvalidParameterException


trait MapAlgebraLeafCodecs {
  implicit def mapAlgebraDecoder: Decoder[MapAlgebraAST]
  implicit def mapAlgebraEncoder: Encoder[MapAlgebraAST]

  /** TODO: Add codec paths besides `raster source` and `operation` when supported */
  implicit def mapAlgebraLeafDecoder = Decoder.instance[MapAlgebraAST.MapAlgebraLeaf] { ma =>
    ma._type match {
      case Some("src") =>
        ma.as[MapAlgebraAST.Source]
      case Some("ref") =>
        ma.as[MapAlgebraAST.ToolReference]
      case Some("const") =>
        ma.as[MapAlgebraAST.Constant]
      case Some("sceneSrc") =>
        ma.as[MapAlgebraAST.SceneRaster]
      case Some("projectSrc") =>
        ma.as[MapAlgebraAST.ProjectRaster]
      case _ =>
        Left(DecodingFailure(s"Unrecognized leaf node: $ma", ma.history))
    }
  }

  implicit def mapAlgebraLeafEncoder: Encoder[MapAlgebraAST.MapAlgebraLeaf] = new Encoder[MapAlgebraAST.MapAlgebraLeaf] {
    final def apply(ast: MapAlgebraAST.MapAlgebraLeaf): Json = ast match {
      case src: MapAlgebraAST.Source =>
        src.asJson
      case reference: MapAlgebraAST.ToolReference =>
        reference.asJson
      case const: MapAlgebraAST.Constant =>
        const.asJson
      case sceneSrc: MapAlgebraAST.SceneRaster =>
        sceneSrc.asJson
      case projectSrc: MapAlgebraAST.ProjectRaster =>
        projectSrc.asJson
      case _ =>
        throw new InvalidParameterException(s"Unrecognized AST: $ast")
    }
  }

  implicit lazy val decodeSource: Decoder[MapAlgebraAST.Source] = Decoder.instance[MapAlgebraAST.Source] { _ =>
    Right(MapAlgebraAST.Source())
  }
  implicit lazy val encodeSource: Encoder[MapAlgebraAST.Source] =
    Encoder.forProduct1("type")(src => (src.`type`))

  implicit lazy val decodeSceneSource: Decoder[MapAlgebraAST.SceneRaster] =
    Decoder.forProduct3("sceneId", "band", "celltype")(MapAlgebraAST.SceneRaster.apply)
  implicit lazy val encodeSceneSource: Encoder[MapAlgebraAST.SceneRaster] =
    Encoder.forProduct4("type", "sceneId", "band", "celltype")(src => (src.`type`, src.sceneId, src.band, src.celltype))

  implicit lazy val decodeProjectSource: Decoder[MapAlgebraAST.ProjectRaster] =
    Decoder.forProduct3("projId", "band", "celltype")(MapAlgebraAST.ProjectRaster.apply)
  implicit lazy val encodeProjectSource: Encoder[MapAlgebraAST.ProjectRaster] =
    Encoder.forProduct4("type", "projId", "band", "celltype")(src => (src.`type`, src.projId, src.band, src.celltype))

  implicit lazy val decodeConstant: Decoder[MapAlgebraAST.Constant] =
    Decoder.forProduct1("constant")(MapAlgebraAST.Constant.apply)
  implicit lazy val encodeConstant: Encoder[MapAlgebraAST.Constant] =
    Encoder.forProduct2("type", "constant")(const => (const.`type`, const.constant))

  implicit lazy val decodeReference: Decoder[MapAlgebraAST.ToolReference] =
    Decoder.forProduct1("toolId")(MapAlgebraAST.ToolReference.apply)
  implicit lazy val encodeReference: Encoder[MapAlgebraAST.ToolReference] =
    Encoder.forProduct2("type", "toolId")(ref => (ref.`type`, ref.toolId))

  implicit lazy val celltypeDecoder: Decoder[CellType] =
    Decoder[String].map({ CellType.fromName(_) })
  implicit lazy val celltypeEncoder: Encoder[CellType] =
    Encoder.encodeString.contramap[CellType]({ CellType.toName(_) })
}

