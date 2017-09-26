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
object BufferingInterpreter extends LazyLogging {

  def literalize(
    ast: MapAlgebraAST,
    tileSource: (RFMLRaster, Boolean, Int, Int, Int) => Future[Interpreted[TileWithNeighbors]],
    z: Int,
    x: Int,
    y: Int
  )(implicit ec: ExecutionContext): Future[Interpreted[MapAlgebraAST]] = {

    def eval(ast: MapAlgebraAST, buffer: Int): Future[Interpreted[MapAlgebraAST]] =
      Try({
        ast match {
          case sr@SceneRaster(_, band, celltype) =>
            if (buffer > 0)
              tileSource(sr, true, z, x, y).map({ interp =>
                interp.map({ tile =>
                  LiteralRaster(tile.withBuffer(buffer)).withId(sr.id)
                })
              })
            else
              tileSource(sr, true, z, x, y).map({ interp =>
                interp.map({ tile =>
                  LiteralRaster(tile.centerTile).withId(sr.id)
                })
              })
          case pr@ProjectRaster(_, band, celltype) =>
            if (buffer > 0)
              tileSource(pr, true, z, x, y).map({ interp =>
                interp.map({ tile =>
                  LiteralRaster(tile.withBuffer(buffer)).withId(pr.id)
                })
              })
            else
              tileSource(pr, true, z, x, y).map({ interp =>
                interp.map({ tile =>
                  LiteralRaster(tile.centerTile).withId(pr.id)
                })
              })
          case f: FocalOperation =>
            ast.args.map(eval(_, buffer + f.neighborhood.extent))
              .sequence
              .map(_.sequence)
              .map({
                case Valid(args) => Valid(ast.withArgs(args))
                case i@Invalid(_) => i
              })
          case _ =>
            ast.args.map(eval(_, buffer))
              .sequence
              .map(_.sequence)
              .map({
                case Valid(args) => Valid(ast.withArgs(args))
                case i@Invalid(_) => i
              })
        }
      }).getOrElse(Future.successful(Invalid(NEL.of(RasterRetrievalError(ast)))))

    // Aggregate multiple errors here...
    eval(ast, 0).map({ res =>
      val pure: Interpreted[Unit] = PureInterpreter.interpret[Unit](ast, false)
      res.leftMap({ errors =>
        pure match {
          case Invalid(e) => e ++ errors.toList
          case _ => errors
        }
      })
    })
  }


  /** The Interpreter method for producing z/x/y TMS tiles
    *
    * @param ast     A [[MapAlgebraAST]] which defines transformations over arbitrary rasters
    * @param source  A function from an [[RFMLRaster]] and z/x/y (tms) integers to possibly
    *                 existing tiles
    */
  def interpret(
    ast: MapAlgebraAST,
    expectedTileSize: Int
  ): (Int, Int, Int) => Interpreted[LazyTile] = {

    (z: Int, x: Int, y: Int) => {
      lazy val extent = TileLayouts(z).mapTransform(SpatialKey(x,y))

      @SuppressWarnings(Array("TraversableHead"))
      def eval(ast: MapAlgebraAST, buffer: Int): LazyTile = {
        logger.debug(s"case ${ast.symbol} at ${ast.id}")
        ast match {

          /* --- LEAVES --- */
          case Source() => sys.error("TMS: Attempt to evaluate a ToolReference!")
          case ToolReference(_) => sys.error("TMS: Attempt to evaluate a ToolReference!")
          case SceneRaster(_, _, _) => sys.error("TMS: Attempt to evaluate a SceneRaster!")
          case ProjectRaster(_, _, _) => sys.error("TMS: Attempt to evaluate a ProjectRaster!")
          case LiteralRaster(lt) => lt
          case Constant(const) => LazyTile.Constant(const)
          /* --- LOCAL OPERATIONS --- */
          case Addition(args) =>
            args.map(eval(_, buffer)).reduce(_ + _)
          case Subtraction(args) =>
            args.map(eval(_, buffer)).reduce(_ - _)
          case Multiplication(args) =>
            args.map(eval(_, buffer)).reduce(_ * _)
          case Division(args) =>
            args.map(eval(_, buffer)).reduce(_ / _)
          case Max(args) =>
            args.map(eval(_, buffer)).reduce(_ max _)
          case Min(args) =>
            args.map(eval(_, buffer)).reduce(_ min _)
          case Classification(args, breaks) =>
            eval(args.head, buffer).classify(breaks.toBreakMap)
          case Masking(args, mask) =>
            eval(args.head, buffer).mask(extent, mask)
          case Equality(args) =>
            args.map(eval(_, buffer)).reduce(_ == _)
          case Inequality(args) =>
            args.map(eval(_, buffer)).reduce(_ != _)
          case Greater(args) =>
            args.map(eval(_, buffer)).reduce(_ > _)
          case GreaterOrEqual(args) =>
            args.map(eval(_, buffer)).reduce(_ >= _)
          case Less(args) =>
            args.map(eval(_, buffer)).reduce(_ < _)
          case LessOrEqual(args) =>
            args.map(eval(_, buffer)).reduce(_ <= _)
          case And(args) =>
            args.map(eval(_, buffer)).reduce(_ and _)
          case Or(args) =>
            args.map(eval(_, buffer)).reduce(_ or _)
          case Xor(args) =>
            args.map(eval(_, buffer)).reduce(_ xor _)
          case Pow(args) =>
            args.map(eval(_, buffer)).reduce(_ ** _)
          case Atan2(args) =>
            args.map(eval(_, buffer)).reduce(_ atan2 _)

          /* --- Unary Operations --- */
          case IsDefined(args) =>
            eval(args.head, buffer).defined
          case IsUndefined(args) =>
            eval(args.head, buffer).undefined
          case SquareRoot(args) =>
            eval(args.head, buffer).sqrt
          case Log(args) =>
            eval(args.head, buffer).log
          case Log10(args) =>
            eval(args.head, buffer).log10
          case Round(args) =>
            eval(args.head, buffer).round
          case Floor(args) =>
            eval(args.head, buffer).floor
          case Ceil(args) =>
            eval(args.head, buffer).ceil
          case NumericNegation(args) =>
            eval(args.head, buffer).inverse
          case LogicalNegation(args) =>
            eval(args.head, buffer).not
          case Abs(args) =>
            eval(args.head, buffer).abs
          case Sin(args) =>
            eval(args.head, buffer).sin
          case Cos(args) =>
            eval(args.head, buffer).cos
          case Tan(args) =>
            eval(args.head, buffer).tan
          case Sinh(args) =>
            eval(args.head, buffer).sinh
          case Cosh(args) =>
            eval(args.head, buffer).cosh
          case Tanh(args) =>
            eval(args.head, buffer).tanh
          case Asin(args) =>
            eval(args.head, buffer).asin
          case Acos(args) =>
            eval(args.head, buffer).acos
          case Atan(args) =>
            eval(args.head, buffer).atan

          /* --- FOCAL OPERATIONS --- */
          case FocalMax(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalMax(n, Some(gridbounds))
          case FocalMin(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalMin(n, Some(gridbounds))
          case FocalMean(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalMean(n, Some(gridbounds))
          case FocalMedian(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalMedian(n, Some(gridbounds))
          case FocalMode(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalMode(n, Some(gridbounds))
          case FocalSum(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalSum(n, Some(gridbounds))
          case FocalStdDev(args, n) =>
            val gridbounds = GridBounds(n.extent, n.extent, expectedTileSize - 1 + buffer * 2 + n.extent, expectedTileSize - 1 + buffer * 2 + n.extent)
            eval(args.head, buffer + n.extent)
              .focalStdDev(n, Some(gridbounds))
        }
      }

      val pure: Interpreted[Unit] = PureInterpreter.interpret[Unit](ast, false)

      pure.map({ case _ => eval(ast, 0) })
    }
  }
}
