package com.azavea.rf.tool.eval

import java.util.UUID

import scala.concurrent.{ExecutionContext, Future}

import cats._
import cats.data.{NonEmptyList => NEL, _}
import cats.data.Validated._
import cats.implicits._
import com.azavea.rf.tool.ast._
import com.azavea.rf.tool.ast.MapAlgebraAST._
import com.typesafe.scalalogging.LazyLogging
import geotrellis.proj4.WebMercator
import geotrellis.raster._
import geotrellis.spark.SpatialKey
import geotrellis.spark.tiling._
import geotrellis.vector.{Extent, MultiPolygon}

import scala.util.Try


/** This interpreter handles resource resolution and compilation of MapAlgebra ASTs */
object GlobalInterpreter extends LazyLogging {

  def literalize(
    ast: MapAlgebraAST,
    extent: Extent,
    tileSource: RFMLRaster => Future[Interpreted[TileWithNeighbors]]
  )(implicit ec: ExecutionContext): Future[Interpreted[MapAlgebraAST]]= {

    def eval(ast: MapAlgebraAST): Future[Interpreted[MapAlgebraAST]] =
      Try({
        ast match {
          case sr@SceneRaster(sceneId, band, celltype) =>
            tileSource(sr).map({ interp =>
              interp.map({ tile =>
                LiteralRaster(tile.centerTile)
              })
            })
          case pr@ProjectRaster(projId, band, celltype) =>
            tileSource(pr).map({ interp =>
              interp.map({ tile =>
                LiteralRaster(tile.centerTile)
              })
            })
          case _ =>
            ast.args.map(eval(_))
              .sequence
              .map(_.sequence)
              .map({
                case Valid(args) => Valid(ast.withArgs(args))
                case i@Invalid(_) => i
              })
        }
      }).getOrElse(Future.successful(Invalid(NEL.of(RasterRetrievalError(ast)))))

    // Aggregate multiple errors here...
    eval(ast).map({ res =>
      val pure: Interpreted[Unit] = PureInterpreter.interpret[Unit](ast, false)
      res.leftMap({ errors =>
        pure match {
          case Invalid(e) => e ++ errors.toList
          case _ => errors
        }
      })
    })
  }

   /*
    * @param ast     A [[MapAlgebraAST]] which defines transformations over arbitrary rasters
    * @param source  A function from an [[RFMLRaster]] and z/x/y (tms) integers to possibly
    *                 existing tiles
    */
  def interpret(
    ast: MapAlgebraAST,
    extent: Extent
  ): Interpreted[LazyTile] = {

    @SuppressWarnings(Array("TraversableHead"))
    def eval(ast: MapAlgebraAST): LazyTile = {
      logger.debug(s"case ${ast.symbol} at ${ast.id}")
      ast match {
        /* --- LEAVES --- */
        case Constant(const) => LazyTile.Constant(const)
        case Source() => sys.error("Attempt to evaluate a variable node!")
        case ToolReference(_) => sys.error("Attempt to evaluate a ToolReference!")
        case SceneRaster(_, _, _) => sys.error("TMS: Attempt to evaluate a SceneRaster!")
        case ProjectRaster(_, _, _) => sys.error("TMS: Attempt to evaluate a ProjectRaster!")
        case LiteralRaster(lt) => lt

        /* --- LOCAL OPERATIONS --- */
        case Addition(args) =>
          args.map(eval(_)).reduce(_ + _)
        case Subtraction(args) =>
          args.map(eval(_)).reduce(_ - _)
        case Multiplication(args) =>
          args.map(eval(_)).reduce(_ * _)
        case Division(args) =>
          args.map(eval(_)).reduce(_ / _)
        case Max(args) =>
          args.map(eval(_)).reduce(_ max _)
        case Min(args) =>
          args.map(eval(_)).reduce(_ min _)
        case Classification(args, breaks) =>
          eval(args.head).classify(breaks.toBreakMap)
        case Masking(args, mask) =>
          eval(args.head).mask(extent, mask)
        case Equality(args) =>
          args.map(eval(_)).reduce(_ == _)
        case Inequality(args) =>
          args.map(eval(_)).reduce(_ != _)
        case Greater(args) =>
          args.map(eval(_)).reduce(_ > _)
        case GreaterOrEqual(args) =>
          args.map(eval(_)).reduce(_ >= _)
        case Less(args) =>
          args.map(eval(_)).reduce(_ < _)
        case LessOrEqual(args) =>
          args.map(eval(_)).reduce(_ <= _)
        case And(args) =>
          args.map(eval(_)).reduce(_ and _)
        case Or(args) =>
          args.map(eval(_)).reduce(_ or _)
        case Xor(args) =>
          args.map(eval(_)).reduce(_ xor _)
        case Pow(args) =>
          args.map(eval(_)).reduce(_ ** _)
        case Atan2(args) =>
          args.map(eval(_)).reduce(_ atan2 _)

        /* --- Unary Operations --- */
        case IsDefined(args) =>
          eval(args.head).defined
        case IsUndefined(args) =>
          eval(args.head).undefined
        case SquareRoot(args) =>
          eval(args.head).sqrt
        case Log(args) =>
          eval(args.head).log
        case Log10(args) =>
          eval(args.head).log10
        case Round(args) =>
          eval(args.head).round
        case Floor(args) =>
          eval(args.head).floor
        case Ceil(args) =>
          eval(args.head).ceil
        case NumericNegation(args) =>
          eval(args.head).inverse
        case LogicalNegation(args) =>
          eval(args.head).not
        case Abs(args) =>
          eval(args.head).abs
        case Sin(args) =>
          eval(args.head).sin
        case Cos(args) =>
          eval(args.head).cos
        case Tan(args) =>
          eval(args.head).tan
        case Sinh(args) =>
          eval(args.head).sinh
        case Cosh(args) =>
          eval(args.head).cosh
        case Tanh(args) =>
          eval(args.head).tanh
        case Asin(args) =>
          eval(args.head).asin
        case Acos(args) =>
          eval(args.head).acos
        case Atan(args) =>
          eval(args.head).atan

        /* --- FOCAL OPERATIONS --- */
        case FocalMax(args, neighborhood) =>
          eval(args.head).focalMax(neighborhood, None)
        case FocalMin(args, neighborhood) =>
          eval(args.head).focalMin(neighborhood, None)
        case FocalMean(args, neighborhood) =>
          eval(args.head).focalMean(neighborhood, None)
        case FocalMedian(args, neighborhood) =>
          eval(args.head).focalMedian(neighborhood, None)
        case FocalMode(args, neighborhood) =>
          eval(args.head).focalMode(neighborhood, None)
        case FocalSum(args, neighborhood) =>
          eval(args.head).focalSum(neighborhood, None)
        case FocalStdDev(args, neighborhood) =>
          eval(args.head).focalStdDev(neighborhood, None)
      }

    }

    val pure: Interpreted[Unit] = PureInterpreter.interpret[Unit](ast, false)
    pure.map({ case _ => eval(ast) })
  }
}

