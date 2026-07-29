package image4s.geometry

sealed trait GeometryError derives CanEqual:
  def message: String

object GeometryError:
  final case class InvalidFrameId(value: String) extends GeometryError:
    val message: String =
      "frame identifier must be non-empty and contain no surrounding whitespace"

  final case class InvalidGridId(value: String) extends GeometryError:
    val message: String =
      "grid identifier must be non-empty and contain no surrounding whitespace"

  final case class InvalidFrameLabel(value: String) extends GeometryError:
    val message: String =
      "frame label must be non-empty and contain no surrounding whitespace"

  final case class DimensionMismatch(expected: Int, actual: Int)
      extends GeometryError:
    val message: String = s"expected spatial rank $expected, got $actual"

  final case class UnsupportedSpatialRank(actual: Int) extends GeometryError:
    val message: String =
      s"only spatial ranks 2 and 3 are supported, got $actual"

  final case class NonFiniteCoordinate(axis: Int, value: Double)
      extends GeometryError:
    val message: String =
      s"coordinate $axis must be finite, got $value"

  final case class InvalidAffineTolerance(value: Double, maximum: Double)
      extends GeometryError:
    val message: String =
      s"affine tolerance must be finite and between zero and $maximum, got $value"

  final case class InvalidAffineShape(expected: Int, actual: Int)
      extends GeometryError:
    val message: String =
      s"expected $expected row-major affine values, got $actual"

  final case class NonFiniteAffineElement(index: Int, value: Double)
      extends GeometryError:
    val message: String =
      s"affine element $index must be finite, got $value"

  final case class InvalidHomogeneousBottomRow(
      actual: Vector[Double],
      tolerance: Double
  ) extends GeometryError:
    val message: String =
      s"affine bottom row ${actual.mkString("[", ", ", "]")} is not " +
        s"[0, ..., 0, 1] within tolerance $tolerance"

  final case class NonInvertibleAffine(diagnostics: String)
      extends GeometryError:
    val message: String =
      s"affine matrix must be invertible: $diagnostics"

  final case class InvalidDirectionShape(expected: Int, actual: Int)
      extends GeometryError:
    val message: String =
      s"expected $expected row-major direction values, got $actual"

  final case class NonFiniteOrigin(axis: Int, value: Double)
      extends GeometryError:
    val message: String =
      s"origin $axis must be finite, got $value"

  final case class InvalidSpacing(axis: Int, value: Double)
      extends GeometryError:
    val message: String =
      s"spacing $axis must be finite and positive, got $value"

  final case class NonFiniteDirection(index: Int, value: Double)
      extends GeometryError:
    val message: String =
      s"direction element $index must be finite, got $value"

  final case class NonPositiveGridExtent(axis: Int, value: Int)
      extends GeometryError:
    val message: String =
      s"grid extent $axis must be positive, got $value"

  final case class FrameMismatch(expected: FrameId, actual: FrameId)
      extends GeometryError:
    val message: String =
      s"expected frame ${expected.value}, got ${actual.value}"

  final case class GridMismatch(expected: GridId, actual: GridId)
      extends GeometryError:
    val message: String =
      s"expected grid ${expected.value}, got ${actual.value}"

  final case class FrameOwnerMismatch(id: FrameId) extends GeometryError:
    val message: String =
      s"frame ${id.value} is represented by distinct live runtime owners"

  final case class GridOwnerMismatch(id: GridId) extends GeometryError:
    val message: String =
      s"grid ${id.value} is represented by distinct live runtime owners"

  final case class FrameRestoreMetadataConflict(id: FrameId)
      extends GeometryError:
    val message: String =
      s"frame ${id.value} is registered with different dimension or metadata"

  final case class FrameRestoreDuplicateOwner(id: FrameId)
      extends GeometryError:
    val message: String =
      s"frame ${id.value} is already registered to another live owner"

  final case class GridRestoreMetadataConflict(id: GridId)
      extends GeometryError:
    val message: String =
      s"grid ${id.value} is registered with different frame, shape, or affine metadata"

  final case class GridRestoreDuplicateOwner(id: GridId)
      extends GeometryError:
    val message: String =
      s"grid ${id.value} is already registered to another live owner"

  final case class GridRestoreFrameOwnerConflict(
      gridId: GridId,
      frameId: FrameId
  ) extends GeometryError:
    val message: String =
      s"grid ${gridId.value} is registered against another live owner of frame ${frameId.value}"
