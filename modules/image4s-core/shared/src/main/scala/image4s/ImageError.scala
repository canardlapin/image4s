package image4s

import image4s.geometry.GeometryError

sealed trait ImageError derives CanEqual:
  def message: String

object ImageError:
  final case class InvalidAxisName(value: String) extends ImageError:
    val message: String =
      "axis name must be non-empty and contain no surrounding whitespace"

  final case class NonPositiveAxisExtent(name: String, extent: Int)
      extends ImageError:
    val message: String =
      s"non-spatial axis $name must have positive extent, got $extent"

  final case class DuplicateAxisName(name: String) extends ImageError:
    val message: String =
      s"non-spatial axis name $name occurs more than once"

  final case class SampledShapeMismatch(
      expected: Vector[Int],
      actual: Vector[Int]
  ) extends ImageError:
    val message: String =
      s"sampled data shape ${actual.mkString("x")} does not match " +
        s"grid and non-spatial axes ${expected.mkString("x")}"

  final case class SpatialIndexRankMismatch(expected: Int, actual: Int)
      extends ImageError:
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

  final case class StorageRankMismatch(expected: Int, actual: Int)
      extends ImageError:
    val message: String =
      s"expected sampled storage rank $expected, got $actual"

  final case class SpatialDimensionMismatch(expected: Int, actual: Int)
      extends ImageError:
    val message: String =
      s"expected sampled spatial rank $expected, got $actual"

  final case class NonSpatialAxisOutOfBounds(index: Int, axisCount: Int)
      extends ImageError:
    val message: String =
      s"non-spatial axis $index is outside $axisCount declared axes"

  final case class MissingNonSpatialAxisKind(kind: AxisKind)
      extends ImageError:
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

  final case class NonPositiveSpatialViewExtent(axis: Int, extent: Int)
      extends ImageError:
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

  final case class OutsideGrid(continuousIndex: Vector[Double])
      extends ImageError:
    val message: String =
      s"continuous index ${continuousIndex.mkString("(", ", ", ")")} is outside the grid"

  final case class InvalidPartialWeight(value: Double) extends ImageError:
    val message: String =
      "partial validity weight must be finite and strictly between zero " +
        s"and one, got $value"

  final case class Geometry(error: GeometryError) extends ImageError:
    val message: String = error.message
