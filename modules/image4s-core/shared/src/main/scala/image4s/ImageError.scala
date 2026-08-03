package image4s

import image4s.geometry.GeometryError
import image4s.geometry.GridRecord

sealed trait ImageError derives CanEqual:
  def message: String

object ImageError:
  final case class ValueEncoding(
      cause: EncodingError
  ) extends ImageError:
    val message: String =
      cause.message

  final case class NumericConversion(
      cause: ravel.ConversionError
  ) extends ImageError:
    val message: String =
      cause.message

  final case class InvalidAxisName(value: String) extends ImageError:
    val message: String =
      "axis name must be non-empty and contain no surrounding whitespace"

  final case class InvalidAxisKindId(value: String) extends ImageError:
    val message: String =
      "custom axis kind identifiers must start with a lowercase ASCII letter " +
        "and contain only lowercase letters, digits, '-', '_', '.', ':', or '/'"

  final case class ReservedAxisKindId(value: String) extends ImageError:
    val message: String =
      s"custom axis kind identifier '$value' is reserved by image4s"

  final case class InvalidAxisUnitId(value: String) extends ImageError:
    val message: String =
      "custom axis unit identifiers must start with a lowercase ASCII letter " +
        "and contain only lowercase letters, digits, '-', '_', '.', ':', or '/'"

  final case class ReservedAxisUnitId(value: String) extends ImageError:
    val message: String =
      s"custom axis unit identifier '$value' is reserved by image4s"

  final case class NonPositiveAxisExtent(name: String, extent: Int) extends ImageError:
    val message: String =
      s"non-spatial axis $name must have positive extent, got $extent"

  final case class NonFiniteAxisOrigin(name: String, value: Double) extends ImageError:
    val message: String =
      s"non-spatial axis $name must have a finite coordinate origin, got $value"

  final case class InvalidAxisStep(name: String, value: Double) extends ImageError:
    val message: String =
      s"non-spatial axis $name must have a finite nonzero coordinate step, got $value"

  final case class NonFiniteAxisCoordinate(
      name: String,
      index: Int,
      value: Double
  ) extends ImageError:
    val message: String =
      s"non-spatial axis $name coordinate $index must be finite, got $value"

  final case class InvalidCategoricalAxisLabel(
      name: String,
      index: Int,
      value: String
  ) extends ImageError:
    val message: String =
      s"non-spatial axis $name categorical label $index must be non-empty " +
        s"and contain no surrounding whitespace, got '$value'"

  final case class DuplicateAxisName(name: String) extends ImageError:
    val message: String =
      s"non-spatial axis name $name occurs more than once"

  final case class SamplingSpecificationRankMismatch(
      spatialRank: Int,
      axisCount: Int,
      actualRank: Int
  ) extends ImageError:
    val message: String =
      s"sampling specification requires storage rank ${spatialRank + axisCount} " +
        s"($spatialRank spatial and $axisCount non-spatial), got $actualRank"

  final case class AxisSpecificationExtentMismatch(
      axis: Int,
      name: String,
      declaredExtent: Int,
      boundExtent: Int
  ) extends ImageError:
    val message: String =
      s"non-spatial axis $axis ($name) declares extent $declaredExtent but " +
        s"the corresponding storage extent is $boundExtent"

  final case class SampledShapeMismatch(
      expected: Vector[Int],
      actual: Vector[Int]
  ) extends ImageError:
    val message: String =
      s"sampled data shape ${actual.mkString("x")} does not match " +
        s"grid and non-spatial axes ${expected.mkString("x")}"

  final case class SampleSpaceGridRecordMismatch(
      expected: GridRecord,
      actual: GridRecord
  ) extends ImageError:
    val message: String =
      s"sample-space grid record ${actual.key.id.value} does not match " +
        s"declared grid record ${expected.key.id.value}"

  final case class PersistentSampleSpaceUnavailable(
      leftEphemeral: Boolean,
      rightEphemeral: Boolean
  ) extends ImageError:
    val message: String =
      s"persistent sample-space comparison requires persistent grids; " +
        s"left ephemeral=$leftEphemeral, right ephemeral=$rightEphemeral"

  final case class NonSpatialSamplingMismatch(
      left: Vector[AxisRecord],
      right: Vector[AxisRecord]
  ) extends ImageError:
    val message: String =
      s"ordered non-spatial sampling differs: left=$left, right=$right"

  final case class SpatialIndexRankMismatch(expected: Int, actual: Int) extends ImageError:
    val message: String =
      s"expected $expected spatial indices, got $actual"

  final case class NonSpatialIndexRankMismatch(
      expected: Int,
      actual: Int
  ) extends ImageError:
    val message: String =
      s"expected $expected non-spatial indices, got $actual"

  final case class SpatialIndexOutOfBounds(
      axis: Int,
      index: Int,
      extent: Int
  ) extends ImageError:
    val message: String =
      s"spatial index $index is outside axis $axis of extent $extent"

  final case class NonSpatialIndexOutOfBounds(
      axis: AxisName,
      index: Int,
      extent: Int
  ) extends ImageError:
    val message: String =
      s"index $index is outside non-spatial axis ${axis.value} of extent $extent"

  final case class StorageRankMismatch(expected: Int, actual: Int) extends ImageError:
    val message: String =
      s"expected sampled storage rank $expected, got $actual"

  final case class SpatialDimensionMismatch(expected: Int, actual: Int) extends ImageError:
    val message: String =
      s"expected sampled spatial rank $expected, got $actual"

  final case class NonSpatialAxisOutOfBounds(index: Int, axisCount: Int) extends ImageError:
    val message: String =
      s"non-spatial axis $index is outside $axisCount declared axes"

  final case class NonSpatialAxisPermutationRankMismatch(
      expected: Int,
      actual: Int
  ) extends ImageError:
    val message: String =
      s"non-spatial axis permutation requires $expected entries, got $actual"

  final case class InvalidNonSpatialAxisPermutation(
      order: Vector[Int],
      axisCount: Int
  ) extends ImageError:
    val message: String =
      s"non-spatial axis permutation ${order.mkString("[", ", ", "]")} must " +
        s"contain every index from 0 until $axisCount exactly once"

  final case class MissingNonSpatialAxisKind(kind: AxisKind) extends ImageError:
    val message: String =
      s"sampled image has no non-spatial axis of kind $kind"

  final case class AmbiguousNonSpatialAxisKind(
      kind: AxisKind,
      count: Int
  ) extends ImageError:
    val message: String =
      s"sampled image has $count non-spatial axes of kind $kind"

  final case class SpatialViewRankMismatch(
      expected: Int,
      originRank: Int,
      shapeRank: Int
  ) extends ImageError:
    val message: String =
      s"spatial view requires origin and shape rank $expected, got " +
        s"$originRank and $shapeRank"

  final case class NonPositiveSpatialViewExtent(axis: Int, extent: Int) extends ImageError:
    val message: String =
      s"spatial view axis $axis must have positive extent, got $extent"

  final case class SpatialViewOutOfBounds(
      axis: Int,
      origin: Int,
      extent: Int,
      sourceExtent: Int
  ) extends ImageError:
    val message: String =
      s"spatial view axis $axis origin $origin and extent $extent exceed " +
        s"source extent $sourceExtent"

  final case class LatticeMapRankMismatch(
      expected: Int,
      sourceShapeRank: Int,
      targetShapeRank: Int,
      originRank: Int,
      permutationRank: Int,
      stepRank: Int
  ) extends ImageError:
    val message: String =
      s"lattice map rank $expected requires source shape, target shape, " +
        s"origin, permutation, and step ranks of $expected, got " +
        s"$sourceShapeRank, $targetShapeRank, $originRank, " +
        s"$permutationRank, and $stepRank"

  final case class NonPositiveLatticeMapExtent(
      side: String,
      axis: Int,
      extent: Int
  ) extends ImageError:
    val message: String =
      s"lattice map $side axis $axis must have positive extent, got $extent"

  final case class InvalidLatticeAxisPermutation(
      order: Vector[Int],
      rank: Int
  ) extends ImageError:
    val message: String =
      s"lattice axis permutation ${order.mkString("[", ", ", "]")} must " +
        s"contain every index from 0 until $rank exactly once"

  final case class ZeroLatticeStep(targetAxis: Int) extends ImageError:
    val message: String =
      s"lattice map target axis $targetAxis must have a nonzero step"

  final case class InvalidSpatialStride(axis: Int, step: Int) extends ImageError:
    val message: String =
      s"spatial stride axis $axis must have a positive step, got $step"

  final case class InvalidSpatialAxis(axis: Int, rank: Int) extends ImageError:
    val message: String =
      s"spatial axis $axis is outside rank $rank"

  final case class LatticeMapOutOfBounds(
      sourceAxis: Int,
      first: Long,
      last: Long,
      sourceExtent: Int
  ) extends ImageError:
    val message: String =
      s"lattice map source axis $sourceAxis visits endpoints $first and " +
        s"$last outside extent $sourceExtent"

  final case class LatticeMapSourceShapeMismatch(
      expected: Vector[Int],
      actual: Vector[Int]
  ) extends ImageError:
    val message: String =
      s"lattice map source shape ${actual.mkString("x")} does not match " +
        s"image grid shape ${expected.mkString("x")}"

  final case class LatticeMapCompositionMismatch(
      leftTarget: Vector[Int],
      rightSource: Vector[Int]
  ) extends ImageError:
    val message: String =
      s"lattice maps cannot compose because left target shape " +
        s"${leftTarget.mkString("x")} differs from right source shape " +
        s"${rightSource.mkString("x")}"

  final case class LatticeTargetIndexRankMismatch(
      expected: Int,
      actual: Int
  ) extends ImageError:
    val message: String =
      s"lattice map expected $expected target indices, got $actual"

  final case class LatticeTargetIndexOutOfBounds(
      axis: Int,
      index: Int,
      extent: Int
  ) extends ImageError:
    val message: String =
      s"lattice map target index $index is outside axis $axis of extent $extent"

  final case class OutsideGrid(continuousIndex: Vector[Double]) extends ImageError:
    val message: String =
      s"continuous index ${continuousIndex.mkString("(", ", ", ")")} is outside the grid"

  final case class InvalidPartialWeight(value: Double) extends ImageError:
    val message: String =
      "partial validity weight must be finite and strictly between zero " +
        s"and one, got $value"

  final case class InvalidNumericTolerance(
      absolute: Double,
      relative: Double
  ) extends ImageError:
    val message: String =
      s"numeric tolerances must be finite and non-negative, got " +
        s"absolute=$absolute and relative=$relative"

  final case class Geometry(error: GeometryError) extends ImageError:
    val message: String = error.message
