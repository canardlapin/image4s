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

  final case class InvalidMaximumConditionNumber(value: Double)
      extends GeometryError:
    val message: String =
      s"maximum affine condition number must be finite and at least one, got $value"

  final case class InvalidMaximumInverseResidual(value: Double)
      extends GeometryError:
    val message: String =
      s"maximum affine inverse residual must be finite and non-negative, got $value"

  final case class IllConditionedAffine(
      estimate: Double,
      maximum: Double
  ) extends GeometryError:
    val message: String =
      s"affine condition estimate $estimate exceeds maximum $maximum"

  final case class AffineInverseResidualTooLarge(
      residual: Double,
      maximum: Double
  ) extends GeometryError:
    val message: String =
      s"affine inverse residual $residual exceeds maximum $maximum"

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

  final case class InvalidDirectionTolerance(
      value: Double,
      maximum: Double
  ) extends GeometryError:
    val message: String =
      s"direction tolerance must be finite and between zero and $maximum, got $value"

  final case class NonOrthonormalDirection(
      maximumDeviation: Double,
      tolerance: Double
  ) extends GeometryError:
    val message: String =
      s"direction cosines have orthonormality deviation $maximumDeviation, exceeding $tolerance"

  final case class NonPositiveGridExtent(axis: Int, value: Int)
      extends GeometryError:
    val message: String =
      s"grid extent $axis must be positive, got $value"

  final case class GridIndexOutOfBounds(
      axis: Int,
      value: Int,
      extent: Int
  ) extends GeometryError:
    val message: String =
      s"grid index $value is outside axis $axis with extent $extent"

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

  case object EphemeralFrameMismatch extends GeometryError:
    val message: String =
      "distinct ephemeral frames have no persistent identity alignment"

  case object EphemeralFrameHasNoRecord extends GeometryError:
    val message: String =
      "an ephemeral frame has no persistent record"

  case object CannotRegisterEphemeralFrame extends GeometryError:
    val message: String =
      "an ephemeral frame cannot be registered as persistent"

  final case class FrameKeyConflict(
      id: FrameId,
      registered: FrameKey,
      requested: FrameKey
  ) extends GeometryError:
    val message: String =
      s"frame ${id.value} has conflicting persistent structural keys"

  final case class GridOwnerMismatch(id: GridId) extends GeometryError:
    val message: String =
      s"grid ${id.value} is represented by distinct live runtime owners"

  case object EphemeralGridMismatch extends GeometryError:
    val message: String =
      "distinct ephemeral grids have no persistent identity alignment"

  case object EphemeralGridHasNoRecord extends GeometryError:
    val message: String =
      "an ephemeral grid has no persistent record"

  case object CannotRegisterEphemeralGrid extends GeometryError:
    val message: String =
      "an ephemeral grid cannot be registered as persistent"

  final case class PersistentGridRequiresPersistentFrame(id: GridId)
      extends GeometryError:
    val message: String =
      s"persistent grid ${id.value} requires a persistent frame key"

  final case class FrameRestoreDuplicateOwner(id: FrameId)
      extends GeometryError:
    val message: String =
      s"frame ${id.value} is already registered to another live owner"

  final case class GridFrameKeyMismatch(
      gridId: GridId,
      expected: FrameKey,
      actual: Option[FrameKey]
  ) extends GeometryError:
    val message: String =
      s"grid ${gridId.value} requires frame ${expected.id.value}, but the " +
        "supplied live frame has another or no persistent key"

  final case class GridKeyConflict(
      id: GridId,
      registered: GridKey,
      requested: GridKey
  ) extends GeometryError:
    val message: String =
      s"grid ${id.value} has conflicting persistent structural keys"

  final case class NonCanonicalGridAffineRecord(id: GridId)
      extends GeometryError:
    val message: String =
      s"grid ${id.value} affine record must use the canonical homogeneous row"

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

  final case class InvalidCongruenceTolerance(value: Double)
      extends GeometryError:
    val message: String =
      s"grid congruence tolerance must be finite and non-negative, got $value"

  final case class GridsNotCongruent(tolerance: Double)
      extends GeometryError:
    val message: String =
      s"grids are not geometrically congruent at tolerance $tolerance"
