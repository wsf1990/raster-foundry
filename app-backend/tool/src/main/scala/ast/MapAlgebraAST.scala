package com.azavea.rf.tool.ast

import com.azavea.rf.datamodel.ClassMap
import com.azavea.rf.tool.eval.LazyTile

import io.circe.generic.JsonCodec
import cats.implicits._
import geotrellis.vector.MultiPolygon
import geotrellis.raster._
import geotrellis.raster.mapalgebra.focal._

import java.util.UUID


/** The ur-type for a recursive representation of MapAlgebra operations */
sealed trait MapAlgebraAST extends Product with Serializable {
  var id: Int = Int.MinValue
  def withId(id: Int) = {
    this.id = id
    this
  }
  val symbol: String
  def withIds: MapAlgebraAST = {
    var currentId = 0
    def assignId(ast: MapAlgebraAST): MapAlgebraAST = {
      val toAssign = currentId
      currentId = currentId + 1
      ast match {
        case op: MapAlgebraAST.Operation =>
          val args = op.args.map(assignId(_))
          op.id = toAssign
          op.withArgs(args)
        case leaf: MapAlgebraAST.MapAlgebraLeaf =>
          leaf.id = toAssign
          leaf
      }
    }
    assignId(this)
  }
  def args: List[MapAlgebraAST]

  @SuppressWarnings(Array("TraversableHead"))
  def find(id: Int): Option[MapAlgebraAST] =
    if (this.id == id)
      Some(this)
    else {
      val matches = args.flatMap(_.find(id))
      matches.headOption
    }
  def sources: Seq[MapAlgebraAST.MapAlgebraLeaf]
  def tileSources: Set[RFMLRaster] = {
    val tileList: List[RFMLRaster] = this match {
      case r: RFMLRaster => List(r)
      case l: MapAlgebraAST.MapAlgebraLeaf => List()
      case ast => ast.args.flatMap(_.tileSources)
    }
    tileList.toSet
  }
  def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST
  def bufferedSources(buffered: Boolean = false): Set[Int] = {
    val bufferList: List[Int] = this match {
      case f: MapAlgebraAST.FocalOperation => f.args.flatMap(_.bufferedSources(true))
      case op: MapAlgebraAST.Operation => op.args.flatMap(_.bufferedSources(buffered))
      case src: MapAlgebraAST.Source => if (buffered) List(src.id) else List()
      case leaf: MapAlgebraAST.MapAlgebraLeaf => List()
      case _ => List()
    }
    bufferList.toSet
  }
}

object MapAlgebraAST {
  /** Map Algebra operations (nodes in this tree) */
  sealed trait Operation extends MapAlgebraAST with Serializable {

    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = args.flatMap(_.sources).distinct
  }

  /** Operations which should only have one argument. */
  case class Addition(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "+"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Subtraction(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "-"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Multiplication(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "*"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Division(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "/"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Max(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "max"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Min(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "min"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Equality(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "=="

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Inequality(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "!="

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Greater(args: List[MapAlgebraAST]) extends Operation {
    val symbol = ">"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class GreaterOrEqual(args: List[MapAlgebraAST]) extends Operation {
    val symbol = ">="

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Less(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "<"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class LessOrEqual(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "<="

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class And(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "and"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Or(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "or"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Xor(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "xor"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Pow(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "^"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Atan2(args: List[MapAlgebraAST]) extends Operation {
    val symbol = "atan2"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }


  sealed trait UnaryOperation extends Operation with Serializable

  case class Masking(args: List[MapAlgebraAST], mask: MultiPolygon) extends UnaryOperation {
    val symbol = "mask"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Classification(args: List[MapAlgebraAST], classMap: ClassMap) extends UnaryOperation {
    val symbol = "classify"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class IsDefined(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "isdefined"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class IsUndefined(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "isundefined"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class SquareRoot(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "sqrt"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Log(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "log"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Log10(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "log10"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Round(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "round"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Floor(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "floor"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Ceil(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "ceil"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class NumericNegation(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "neg"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class LogicalNegation(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "not"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Abs(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "abs"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Sin(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "sin"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Cos(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "cos"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Tan(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "tan"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Sinh(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "sinh"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Cosh(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "cosh"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Tanh(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "tanh"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Asin(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "asin"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Acos(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "acos"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class Atan(args: List[MapAlgebraAST]) extends UnaryOperation {
    val symbol = "atan"

    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  sealed trait FocalOperation extends UnaryOperation {
    def neighborhood: Neighborhood
  }

  case class FocalMax(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalMax"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalMin(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalMin"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalMean(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalMean"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalMedian(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalMedian"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalMode(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalMode"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalSum(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalSum"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  case class FocalStdDev(args: List[MapAlgebraAST], neighborhood: Neighborhood) extends FocalOperation {
    val symbol = "focalStdDev"
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = copy(args = newArgs)
  }

  sealed trait MapAlgebraLeaf extends MapAlgebraAST {
    val symbol: String
    def args: List[MapAlgebraAST] = List.empty
    def withArgs(newArgs: List[MapAlgebraAST]): MapAlgebraAST = this
  }

  case class Constant(constant: Double) extends MapAlgebraLeaf {
    val symbol = "const"
    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = List()
  }

  /** Map Algebra sources */
  case class Source() extends MapAlgebraLeaf {
    val symbol = "src"
    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = List(this)
  }

  case class LiteralRaster(lt: LazyTile) extends MapAlgebraLeaf {
    val symbol = "rasterLiteral"
    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = List(this)
  }

  case class SceneRaster(sceneId: UUID, band: Option[Int], celltype: Option[CellType]) extends MapAlgebraLeaf with RFMLRaster {
    val symbol = "sceneSrc"
    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = List(this)
  }

  case class ProjectRaster(projId: UUID, band: Option[Int], celltype: Option[CellType]) extends MapAlgebraLeaf with RFMLRaster {
    val symbol = "projectSrc"
    def sources: Seq[MapAlgebraAST.MapAlgebraLeaf] = List(this)
  }

  case class ToolReference(toolId: UUID) extends MapAlgebraLeaf {
    val symbol = "ref"
    def sources: List[MapAlgebraAST.MapAlgebraLeaf] = List()
  }
}

