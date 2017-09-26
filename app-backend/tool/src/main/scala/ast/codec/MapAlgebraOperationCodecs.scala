package com.azavea.rf.tool.ast.codec

import com.azavea.rf.tool.ast._
import com.azavea.rf.tool.eval._
import com.azavea.rf.bridge._
import io.circe._
import io.circe.syntax._

import java.security.InvalidParameterException


trait MapAlgebraOperationCodecs extends MapAlgebraUtilityCodecs {
  implicit def mapAlgebraDecoder: Decoder[MapAlgebraAST]
  implicit def mapAlgebraEncoder: Encoder[MapAlgebraAST]

  // Codec routing for Operations
  implicit lazy val decodeOperations = Decoder.instance[MapAlgebraAST.Operation] { ma =>
    ma._symbol match {
      case Some("+") => ma.as[MapAlgebraAST.Addition]
      case Some("-") => ma.as[MapAlgebraAST.Subtraction]
      case Some("/") => ma.as[MapAlgebraAST.Division]
      case Some("*") => ma.as[MapAlgebraAST.Multiplication]
      case Some("mask") => ma.as[MapAlgebraAST.Masking]
      case Some("min") => ma.as[MapAlgebraAST.Min]
      case Some("max") => ma.as[MapAlgebraAST.Max]
      case Some("classify") => ma.as[MapAlgebraAST.Classification]
      case Some("focalMax") => ma.as[MapAlgebraAST.FocalMax]
      case Some("focalMin") => ma.as[MapAlgebraAST.FocalMin]
      case Some("focalMean") => ma.as[MapAlgebraAST.FocalMean]
      case Some("focalMedian") => ma.as[MapAlgebraAST.FocalMedian]
      case Some("focalMode") => ma.as[MapAlgebraAST.FocalMode]
      case Some("focalSum") => ma.as[MapAlgebraAST.FocalSum]
      case Some("focalStdDev") => ma.as[MapAlgebraAST.FocalStdDev]
      case Some("and") => ma.as[MapAlgebraAST.And]
      case Some("not") => ma.as[MapAlgebraAST.LogicalNegation]
      case Some("or") => ma.as[MapAlgebraAST.Or]
      case Some("xor") => ma.as[MapAlgebraAST.Xor]
      case Some("^") => ma.as[MapAlgebraAST.Pow]
      case Some("abs") => ma.as[MapAlgebraAST.Abs]
      case Some(">") => ma.as[MapAlgebraAST.Greater]
      case Some(">=") => ma.as[MapAlgebraAST.GreaterOrEqual]
      case Some("==") => ma.as[MapAlgebraAST.Equality]
      case Some("!=") => ma.as[MapAlgebraAST.Inequality]
      case Some("<") => ma.as[MapAlgebraAST.Less]
      case Some("<=") => ma.as[MapAlgebraAST.LessOrEqual]
      case Some("log") => ma.as[MapAlgebraAST.Log]
      case Some("log10") => ma.as[MapAlgebraAST.Log10]
      case Some("sqrt") => ma.as[MapAlgebraAST.SquareRoot]
      case Some("round") => ma.as[MapAlgebraAST.Round]
      case Some("ceil") => ma.as[MapAlgebraAST.Ceil]
      case Some("floor") => ma.as[MapAlgebraAST.Floor]
      case Some("neg") => ma.as[MapAlgebraAST.NumericNegation]
      case Some("sin") => ma.as[MapAlgebraAST.Sin]
      case Some("cos") => ma.as[MapAlgebraAST.Cos]
      case Some("tan") => ma.as[MapAlgebraAST.Tan]
      case Some("sinh") => ma.as[MapAlgebraAST.Sinh]
      case Some("cosh") => ma.as[MapAlgebraAST.Cosh]
      case Some("tanh") => ma.as[MapAlgebraAST.Tanh]
      case Some("asin") => ma.as[MapAlgebraAST.Asin]
      case Some("acos") => ma.as[MapAlgebraAST.Acos]
      case Some("atan") => ma.as[MapAlgebraAST.Atan]
      case Some("atan2") => ma.as[MapAlgebraAST.Atan2]
      case Some(unrecognized) =>
        Left(DecodingFailure(s"Unrecognized node type: $unrecognized", ma.history))
      case None =>
        Left(DecodingFailure("The property 'apply' is mandatory on all AST operations", ma.history))
    }
  }

  implicit lazy val encodeOperations: Encoder[MapAlgebraAST.Operation] = new Encoder[MapAlgebraAST.Operation] {
    final def apply(op: MapAlgebraAST.Operation): Json = op match {
      case addition: MapAlgebraAST.Addition =>
        addition.asJson
      case subtraction: MapAlgebraAST.Subtraction =>
        subtraction.asJson
      case division: MapAlgebraAST.Division =>
        division.asJson
      case multiplication: MapAlgebraAST.Multiplication =>
        multiplication.asJson
      case masking: MapAlgebraAST.Masking =>
        masking.asJson
      case min: MapAlgebraAST.Min =>
        min.asJson
      case max: MapAlgebraAST.Max =>
        max.asJson
      case classification: MapAlgebraAST.Classification =>
        classification.asJson
      case fMax: MapAlgebraAST.FocalMax =>
        fMax.asJson
      case fMin: MapAlgebraAST.FocalMin =>
        fMin.asJson
      case fMean: MapAlgebraAST.FocalMean =>
        fMean.asJson
      case fMedian: MapAlgebraAST.FocalMedian =>
        fMedian.asJson
      case fMode: MapAlgebraAST.FocalMode =>
        fMode.asJson
      case fSum: MapAlgebraAST.FocalSum =>
        fSum.asJson
      case fStdDev: MapAlgebraAST.FocalStdDev =>
        fStdDev.asJson
      case and: MapAlgebraAST.And =>
        and.asJson
      case not: MapAlgebraAST.LogicalNegation =>
        not.asJson
      case or: MapAlgebraAST.Or =>
        or.asJson
      case xor: MapAlgebraAST.Xor =>
        xor.asJson
      case pow: MapAlgebraAST.Pow =>
        pow.asJson
      case abs: MapAlgebraAST.Abs =>
        abs.asJson
      case defined: MapAlgebraAST.IsDefined =>
        defined.asJson
      case undefined: MapAlgebraAST.IsUndefined =>
        undefined.asJson
      case greater: MapAlgebraAST.Greater =>
        greater.asJson
      case greaterOrEqual: MapAlgebraAST.GreaterOrEqual =>
        greaterOrEqual.asJson
      case equal: MapAlgebraAST.Equality =>
        equal.asJson
      case unequal: MapAlgebraAST.Inequality =>
        unequal.asJson
      case less: MapAlgebraAST.Less =>
        less.asJson
      case lessOrEqual: MapAlgebraAST.LessOrEqual =>
        lessOrEqual.asJson
      case log: MapAlgebraAST.Log =>
        log.asJson
      case log10: MapAlgebraAST.Log10 =>
        log10.asJson
      case sqrt: MapAlgebraAST.SquareRoot =>
        sqrt.asJson
      case round: MapAlgebraAST.Round =>
        round.asJson
      case ceil: MapAlgebraAST.Ceil =>
        ceil.asJson
      case floor: MapAlgebraAST.Floor =>
        floor.asJson
      case negative: MapAlgebraAST.NumericNegation =>
        negative.asJson
      case sin: MapAlgebraAST.Sin =>
        sin.asJson
      case cos: MapAlgebraAST.Cos =>
        cos.asJson
      case tan: MapAlgebraAST.Tan =>
        tan.asJson
      case sinh: MapAlgebraAST.Sinh =>
        sinh.asJson
      case cosh: MapAlgebraAST.Cosh =>
        cosh.asJson
      case tanh: MapAlgebraAST.Tanh =>
        tanh.asJson
      case asin: MapAlgebraAST.Asin =>
        asin.asJson
      case acos: MapAlgebraAST.Acos =>
        acos.asJson
      case atan: MapAlgebraAST.Atan =>
        atan.asJson
      case atan2: MapAlgebraAST.Atan2 =>
        atan2.asJson
      case operation =>
        throw new InvalidParameterException(s"Encoder for $operation not yet implemented")
    }
  }

  /** NOTE: We need to keep these specialized encoder/decoders around for correct parsing of trees */
  implicit lazy val decodeAddition: Decoder[MapAlgebraAST.Addition] =
    Decoder.forProduct1("args")(MapAlgebraAST.Addition.apply)
  implicit lazy val encodeAddition: Encoder[MapAlgebraAST.Addition] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeSubtraction: Decoder[MapAlgebraAST.Subtraction] =
    Decoder.forProduct1("args")(MapAlgebraAST.Subtraction.apply)
  implicit lazy val encodeSubtraction: Encoder[MapAlgebraAST.Subtraction] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeDivision: Decoder[MapAlgebraAST.Division] =
    Decoder.forProduct1("args")(MapAlgebraAST.Division.apply)
  implicit lazy val encodeDivision: Encoder[MapAlgebraAST.Division] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeMultiplication: Decoder[MapAlgebraAST.Multiplication] =
    Decoder.forProduct1("args")(MapAlgebraAST.Multiplication.apply)
  implicit lazy val encodeMultiplication: Encoder[MapAlgebraAST.Multiplication] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeMasking: Decoder[MapAlgebraAST.Masking] =
    Decoder.forProduct2("args", "mask")(MapAlgebraAST.Masking.apply)
  implicit lazy val encodeMasking: Encoder[MapAlgebraAST.Masking] =
    Encoder.forProduct3("apply", "args", "mask")(op => (op.symbol, op.args, op.mask))

  implicit lazy val decodeClassification: Decoder[MapAlgebraAST.Classification] =
    Decoder.forProduct2("args", "classMap")(MapAlgebraAST.Classification.apply)
  implicit lazy val encodeClassification: Encoder[MapAlgebraAST.Classification] =
    Encoder.forProduct3("apply", "args", "classMap")(op => (op.symbol, op.args, op.classMap))

  implicit lazy val decodeMax: Decoder[MapAlgebraAST.Max] =
    Decoder.forProduct1("args")(MapAlgebraAST.Max.apply)
  implicit lazy val encodeMax: Encoder[MapAlgebraAST.Max] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeMin: Decoder[MapAlgebraAST.Min] =
    Decoder.forProduct1("args")(MapAlgebraAST.Min.apply)
  implicit lazy val encodeMin: Encoder[MapAlgebraAST.Min] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeFocalMax: Decoder[MapAlgebraAST.FocalMax] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalMax.apply)
  implicit lazy val encodeFocalMax: Encoder[MapAlgebraAST.FocalMax] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalMin: Decoder[MapAlgebraAST.FocalMin] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalMin.apply)
  implicit lazy val encodeFocalMin: Encoder[MapAlgebraAST.FocalMin] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalMean: Decoder[MapAlgebraAST.FocalMean] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalMean.apply)
  implicit lazy val encodeFocalMean: Encoder[MapAlgebraAST.FocalMean] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalMedian: Decoder[MapAlgebraAST.FocalMedian] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalMedian.apply)
  implicit lazy val encodeFocalMedian: Encoder[MapAlgebraAST.FocalMedian] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalMode: Decoder[MapAlgebraAST.FocalMode] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalMode.apply)
  implicit lazy val encodeFocalMode: Encoder[MapAlgebraAST.FocalMode] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalSum: Decoder[MapAlgebraAST.FocalSum] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalSum.apply)
  implicit lazy val encodeFocalSum: Encoder[MapAlgebraAST.FocalSum] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeFocalStdDev: Decoder[MapAlgebraAST.FocalStdDev] =
    Decoder.forProduct2("args", "neighborhood")(MapAlgebraAST.FocalStdDev.apply)
  implicit lazy val encodeFocalStdDev: Encoder[MapAlgebraAST.FocalStdDev] =
    Encoder.forProduct3("apply", "args", "neighborhood")(op => (op.symbol, op.args, op.neighborhood))

  implicit lazy val decodeAnd: Decoder[MapAlgebraAST.And] =
    Decoder.forProduct1("args")(MapAlgebraAST.And.apply)
  implicit lazy val encodeAnd: Encoder[MapAlgebraAST.And] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLogicalNegation: Decoder[MapAlgebraAST.LogicalNegation] =
    Decoder.forProduct1("args")(MapAlgebraAST.LogicalNegation.apply)
  implicit lazy val encodeLogicalNegation: Encoder[MapAlgebraAST.LogicalNegation] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLogicalDisjunction: Decoder[MapAlgebraAST.Or] =
    Decoder.forProduct1("args")(MapAlgebraAST.Or.apply)
  implicit lazy val encodeLogicalDisjunction: Encoder[MapAlgebraAST.Or] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLogicallyExclusiveDisjunction: Decoder[MapAlgebraAST.Xor] =
    Decoder.forProduct1("args")(MapAlgebraAST.Xor.apply)
  implicit lazy val encodeLogicallyExclusiveDisjunction: Encoder[MapAlgebraAST.Xor] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodePow: Decoder[MapAlgebraAST.Pow] =
    Decoder.forProduct1("args")(MapAlgebraAST.Pow.apply)
  implicit lazy val encodePow: Encoder[MapAlgebraAST.Pow] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeAbs: Decoder[MapAlgebraAST.Abs] =
    Decoder.forProduct1("args")(MapAlgebraAST.Abs.apply)
  implicit lazy val encodeAbs: Encoder[MapAlgebraAST.Abs] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeGreater: Decoder[MapAlgebraAST.Greater] =
    Decoder.forProduct1("args")(MapAlgebraAST.Greater.apply)
  implicit lazy val encodeGreater: Encoder[MapAlgebraAST.Greater] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeGreaterOrEqual: Decoder[MapAlgebraAST.GreaterOrEqual] =
    Decoder.forProduct1("args")(MapAlgebraAST.GreaterOrEqual.apply)
  implicit lazy val encodeGreaterOrEqual: Encoder[MapAlgebraAST.GreaterOrEqual] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeEquality: Decoder[MapAlgebraAST.Equality] =
    Decoder.forProduct1("args")(MapAlgebraAST.Equality.apply)
  implicit lazy val encodeEquality: Encoder[MapAlgebraAST.Equality] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeInequality: Decoder[MapAlgebraAST.Inequality] =
    Decoder.forProduct1("args")(MapAlgebraAST.Inequality.apply)
  implicit lazy val encodeInequality: Encoder[MapAlgebraAST.Inequality] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLess: Decoder[MapAlgebraAST.Less] =
    Decoder.forProduct1("args")(MapAlgebraAST.Less.apply)
  implicit lazy val encodeLess: Encoder[MapAlgebraAST.Less] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLessOrEqual: Decoder[MapAlgebraAST.LessOrEqual] =
    Decoder.forProduct1("args")(MapAlgebraAST.LessOrEqual.apply)
  implicit lazy val encodeLessOrEqual: Encoder[MapAlgebraAST.LessOrEqual] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLog: Decoder[MapAlgebraAST.Log] =
    Decoder.forProduct1("args")(MapAlgebraAST.Log.apply)
  implicit lazy val encodeLog: Encoder[MapAlgebraAST.Log] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeLog10: Decoder[MapAlgebraAST.Log10] =
    Decoder.forProduct1("args")(MapAlgebraAST.Log10.apply)
  implicit lazy val encodeLog10: Encoder[MapAlgebraAST.Log10] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeSquareRoot: Decoder[MapAlgebraAST.SquareRoot] =
    Decoder.forProduct1("args")(MapAlgebraAST.SquareRoot.apply)
  implicit lazy val encodeSquareRoot: Encoder[MapAlgebraAST.SquareRoot] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeRound: Decoder[MapAlgebraAST.Round] =
    Decoder.forProduct1("args")(MapAlgebraAST.Round.apply)
  implicit lazy val encodeRound: Encoder[MapAlgebraAST.Round] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeCeil: Decoder[MapAlgebraAST.Ceil] =
    Decoder.forProduct1("args")(MapAlgebraAST.Ceil.apply)
  implicit lazy val encodeCeil: Encoder[MapAlgebraAST.Ceil] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeFloor: Decoder[MapAlgebraAST.Floor] =
    Decoder.forProduct1("args")(MapAlgebraAST.Floor.apply)
  implicit lazy val encodeFloor: Encoder[MapAlgebraAST.Floor] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeNumericNegation: Decoder[MapAlgebraAST.NumericNegation] =
    Decoder.forProduct1("args")(MapAlgebraAST.NumericNegation.apply)
  implicit lazy val encodeNumericNegation: Encoder[MapAlgebraAST.NumericNegation] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeSin: Decoder[MapAlgebraAST.Sin] =
    Decoder.forProduct1("args")(MapAlgebraAST.Sin.apply)
  implicit lazy val encodeSin: Encoder[MapAlgebraAST.Sin] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeCos: Decoder[MapAlgebraAST.Cos] =
    Decoder.forProduct1("args")(MapAlgebraAST.Cos.apply)
  implicit lazy val encodeCos: Encoder[MapAlgebraAST.Cos] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeTan: Decoder[MapAlgebraAST.Tan] =
    Decoder.forProduct1("args")(MapAlgebraAST.Tan.apply)
  implicit lazy val encodeTan: Encoder[MapAlgebraAST.Tan] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeSinh: Decoder[MapAlgebraAST.Sinh] =
    Decoder.forProduct1("args")(MapAlgebraAST.Sinh.apply)
  implicit lazy val encodeSinh: Encoder[MapAlgebraAST.Sinh] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeCosh: Decoder[MapAlgebraAST.Cosh] =
    Decoder.forProduct1("args")(MapAlgebraAST.Cosh.apply)
  implicit lazy val encodeCosh: Encoder[MapAlgebraAST.Cosh] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeTanh: Decoder[MapAlgebraAST.Tanh] =
    Decoder.forProduct1("args")(MapAlgebraAST.Tanh.apply)
  implicit lazy val encodeTanh: Encoder[MapAlgebraAST.Tanh] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeAsin: Decoder[MapAlgebraAST.Asin] =
    Decoder.forProduct1("args")(MapAlgebraAST.Asin.apply)
  implicit lazy val encodeAsin: Encoder[MapAlgebraAST.Asin] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeAcos: Decoder[MapAlgebraAST.Acos] =
    Decoder.forProduct1("args")(MapAlgebraAST.Acos.apply)
  implicit lazy val encodeAcos: Encoder[MapAlgebraAST.Acos] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeAtan: Decoder[MapAlgebraAST.Atan] =
    Decoder.forProduct1("args")(MapAlgebraAST.Atan.apply)
  implicit lazy val encodeAtan: Encoder[MapAlgebraAST.Atan] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeAtan2: Decoder[MapAlgebraAST.Atan2] =
    Decoder.forProduct1("args")(MapAlgebraAST.Atan2.apply)
  implicit lazy val encodeAtan2: Encoder[MapAlgebraAST.Atan2] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeIsDefined: Decoder[MapAlgebraAST.IsDefined] =
    Decoder.forProduct1("args")(MapAlgebraAST.IsDefined.apply)
  implicit lazy val encodeIsDefined: Encoder[MapAlgebraAST.IsDefined] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))

  implicit lazy val decodeIsUndefined: Decoder[MapAlgebraAST.IsUndefined] =
    Decoder.forProduct1("args")(MapAlgebraAST.IsUndefined.apply)
  implicit lazy val encodeIsUndefined: Encoder[MapAlgebraAST.IsUndefined] =
    Encoder.forProduct2("apply", "args")(op => (op.symbol, op.args))
}
