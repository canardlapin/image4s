package image4s.geometry

import gale.linalg.DMat

final case class AffineDiagnostics(
    inverseResidualInfinityNorm: Double,
    conditionEstimateInfinityNorm: Double
) derives CanEqual

final class Affine[D <: Dim] private (
    val matrix: DMat,
    private val inverseMatrix: DMat,
    val tolerance: Double,
    val diagnostics: AffineDiagnostics,
    private val maximumConditionNumber: Double,
    private val maximumInverseResidual: Double
):
  def inverse: Affine[D] =
    new Affine(
      inverseMatrix,
      matrix,
      tolerance,
      diagnostics,
      maximumConditionNumber,
      maximumInverseResidual
    )

  /** Compose this coordinate operator with `next`.
    *
    * The returned operator applies this affine first and `next` second.
    */
  def andThen(
      next: Affine[D]
  )(using Dimension[D]): Either[GeometryError, Affine[D]] =
    Affine.compose(this, next)

  def apply(coordinates: Vector[Double])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Vector[Double]] =
    if coordinates.length != dimension.rank then
      Left(
        GeometryError.DimensionMismatch(dimension.rank, coordinates.length)
      )
    else if coordinates.exists(value => !value.isFinite) then
      val axis = coordinates.indexWhere(value => !value.isFinite)
      Left(GeometryError.NonFiniteCoordinate(axis, coordinates(axis)))
    else Right(applyUnchecked(coordinates, matrix, dimension.rank))

  def rowMajor: Vector[Double] =
    Vector.tabulate(matrix.rows * matrix.cols) { flat =>
      matrix(flat / matrix.cols, flat % matrix.cols)
    }

  private[geometry] def applyUnchecked(
      coordinates: Vector[Double]
  )(using dimension: Dimension[D]): Vector[Double] =
    applyUnchecked(coordinates, matrix, dimension.rank)

  private def applyUnchecked(
      coordinates: Vector[Double],
      operator: DMat,
      rank: Int
  ): Vector[Double] =
    Vector.tabulate(rank) { row =>
      var sum = operator(row, rank)
      var column = 0
      while column < rank do
        sum += operator(row, column) * coordinates(column)
        column += 1
      sum
    }

object Affine:
  val DefaultTolerance: Double = 1e-12
  val MaximumValidationTolerance: Double = 1e-6
  val DefaultDirectionTolerance: Double = 1e-8
  val DefaultMaximumConditionNumber: Double = 1e12
  val DefaultMaximumInverseResidual: Double = 1e-9

  def identity[D <: Dim](using dimension: Dimension[D]): Affine[D] =
    val size = dimension.rank + 1
    val matrix = DMat.eye(size)
    new Affine(
      matrix,
      matrix,
      DefaultTolerance,
      AffineDiagnostics(0.0, 1.0),
      DefaultMaximumConditionNumber,
      DefaultMaximumInverseResidual
    )

  def fromRowMajor[D <: Dim](
      values: IterableOnce[Double],
      tolerance: Double = DefaultTolerance,
      maximumConditionNumber: Double = DefaultMaximumConditionNumber,
      maximumInverseResidual: Double = DefaultMaximumInverseResidual
  )(using dimension: Dimension[D]): Either[GeometryError, Affine[D]] =
    val copied = values.iterator.toVector
    val size = dimension.rank + 1
    val expected = size * size
    if !tolerance.isFinite ||
      tolerance < 0.0 ||
      tolerance > MaximumValidationTolerance
    then
      Left(
        GeometryError.InvalidAffineTolerance(
          tolerance,
          MaximumValidationTolerance
        )
      )
    else if !maximumConditionNumber.isFinite || maximumConditionNumber < 1.0
    then
      Left(
        GeometryError.InvalidMaximumConditionNumber(
          maximumConditionNumber
        )
      )
    else if !maximumInverseResidual.isFinite || maximumInverseResidual < 0.0
    then
      Left(
        GeometryError.InvalidMaximumInverseResidual(
          maximumInverseResidual
        )
      )
    else if copied.length != expected then
      Left(GeometryError.InvalidAffineShape(expected, copied.length))
    else if copied.exists(value => !value.isFinite) then
      val index = copied.indexWhere(value => !value.isFinite)
      Left(GeometryError.NonFiniteAffineElement(index, copied(index)))
    else if !hasHomogeneousBottomRow(copied, dimension.rank, tolerance) then
      Left(
        GeometryError.InvalidHomogeneousBottomRow(
          copied.slice(dimension.rank * size, dimension.rank * size + size),
          tolerance
        )
      )
    else
      val canonical = canonicalizeHomogeneousBottomRow(
        copied,
        dimension.rank
      )
      val matrix = DMat.dense(size, size, canonical)
      matrix
        .solve(DMat.eye(size))
        .left
        .map(error => GeometryError.NonInvertibleAffine(error.getMessage))
        .flatMap { inverse =>
          val inverseValues = matrixRowMajor(inverse)
          val diagnostics =
            affineDiagnostics(canonical, inverseValues, size)
          if !diagnostics.conditionEstimateInfinityNorm.isFinite ||
            diagnostics.conditionEstimateInfinityNorm >
              maximumConditionNumber
          then
            Left(
              GeometryError.IllConditionedAffine(
                diagnostics.conditionEstimateInfinityNorm,
                maximumConditionNumber
              )
            )
          else if !diagnostics.inverseResidualInfinityNorm.isFinite ||
            diagnostics.inverseResidualInfinityNorm >
              maximumInverseResidual
          then
            Left(
              GeometryError.AffineInverseResidualTooLarge(
                diagnostics.inverseResidualInfinityNorm,
                maximumInverseResidual
              )
            )
          else
            Right(
              new Affine(
                matrix,
                inverse,
                tolerance,
                diagnostics,
                maximumConditionNumber,
                maximumInverseResidual
              )
            )
        }

  def fromOriginSpacingDirection[D <: Dim](
      origin: IterableOnce[Double],
      spacing: IterableOnce[Double],
      directionRowMajor: IterableOnce[Double],
      tolerance: Double = DefaultTolerance,
      directionTolerance: Double = DefaultDirectionTolerance,
      maximumConditionNumber: Double = DefaultMaximumConditionNumber,
      maximumInverseResidual: Double = DefaultMaximumInverseResidual
  )(using dimension: Dimension[D]): Either[GeometryError, Affine[D]] =
    val originValues = origin.iterator.toVector
    val spacingValues = spacing.iterator.toVector
    val directionValues = directionRowMajor.iterator.toVector
    val rank = dimension.rank
    if originValues.length != rank then
      Left(GeometryError.DimensionMismatch(rank, originValues.length))
    else if spacingValues.length != rank then
      Left(GeometryError.DimensionMismatch(rank, spacingValues.length))
    else if directionValues.length != rank * rank then
      Left(
        GeometryError.InvalidDirectionShape(
          rank * rank,
          directionValues.length
        )
      )
    else if originValues.exists(value => !value.isFinite) then
      val axis = originValues.indexWhere(value => !value.isFinite)
      Left(GeometryError.NonFiniteOrigin(axis, originValues(axis)))
    else if directionValues.exists(value => !value.isFinite) then
      val index = directionValues.indexWhere(value => !value.isFinite)
      Left(
        GeometryError.NonFiniteDirection(index, directionValues(index))
      )
    else if !directionTolerance.isFinite ||
      directionTolerance < 0.0 ||
      directionTolerance > MaximumValidationTolerance
    then
      Left(
        GeometryError.InvalidDirectionTolerance(
          directionTolerance,
          MaximumValidationTolerance
        )
      )
    else if directionOrthonormalDeviation(directionValues, rank) >
        directionTolerance
    then
      Left(
        GeometryError.NonOrthonormalDirection(
          directionOrthonormalDeviation(directionValues, rank),
          directionTolerance
        )
      )
    else if spacingValues.exists(value => !value.isFinite || value <= 0.0) then
      val axis =
        spacingValues.indexWhere(value => !value.isFinite || value <= 0.0)
      Left(GeometryError.InvalidSpacing(axis, spacingValues(axis)))
    else
      val size = rank + 1
      val values = Vector.tabulate(size * size) { flat =>
        val row = flat / size
        val column = flat % size
        if row < rank && column < rank then
          directionValues(row * rank + column) * spacingValues(column)
        else if row < rank && column == rank then originValues(row)
        else if row == rank && column == rank then 1.0
        else 0.0
      }
      fromRowMajor[D](
        values,
        tolerance,
        maximumConditionNumber,
        maximumInverseResidual
      )

  private def compose[D <: Dim](
      first: Affine[D],
      second: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Affine[D]] =
    val size = dimension.rank + 1
    val left = second.rowMajor
    val right = first.rowMajor
    val product = Vector.tabulate(size * size) { flat =>
      val row = flat / size
      val column = flat % size
      var middle = 0
      var sum = 0.0
      while middle < size do
        sum += left(row * size + middle) * right(middle * size + column)
        middle += 1
      sum
    }
    fromRowMajor[D](
      product,
      tolerance = math.max(first.tolerance, second.tolerance),
      maximumConditionNumber = math.min(
        first.maximumConditionNumber,
        second.maximumConditionNumber
      ),
      maximumInverseResidual = math.min(
        first.maximumInverseResidual,
        second.maximumInverseResidual
      )
    )

  private def directionOrthonormalDeviation(
      values: Vector[Double],
      rank: Int
  ): Double =
    var maximum = 0.0
    var leftColumn = 0
    while leftColumn < rank do
      var rightColumn = 0
      while rightColumn < rank do
        var row = 0
        var dot = 0.0
        while row < rank do
          dot +=
            values(row * rank + leftColumn) *
              values(row * rank + rightColumn)
          row += 1
        val expected = if leftColumn == rightColumn then 1.0 else 0.0
        maximum = math.max(maximum, math.abs(dot - expected))
        rightColumn += 1
      leftColumn += 1
    maximum

  private def affineDiagnostics(
      values: Vector[Double],
      inverse: Vector[Double],
      size: Int
  ): AffineDiagnostics =
    val condition =
      linearInfinityNorm(values, size) *
        linearInfinityNorm(inverse, size)
    val residual =
      math.max(
        identityResidual(values, inverse, size),
        identityResidual(inverse, values, size)
      )
    AffineDiagnostics(residual, condition)

  private def linearInfinityNorm(
      values: Vector[Double],
      size: Int
  ): Double =
    val rank = size - 1
    var maximum = 0.0
    var row = 0
    while row < rank do
      var sum = 0.0
      var column = 0
      while column < rank do
        sum += math.abs(values(row * size + column))
        column += 1
      maximum = math.max(maximum, sum)
      row += 1
    maximum

  private def identityResidual(
      left: Vector[Double],
      right: Vector[Double],
      size: Int
  ): Double =
    var maximum = 0.0
    var row = 0
    while row < size do
      var rowSum = 0.0
      var column = 0
      while column < size do
        var middle = 0
        var product = 0.0
        while middle < size do
          product +=
            left(row * size + middle) *
              right(middle * size + column)
          middle += 1
        val expected = if row == column then 1.0 else 0.0
        rowSum += math.abs(product - expected)
        column += 1
      maximum = math.max(maximum, rowSum)
      row += 1
    maximum

  private def matrixRowMajor(matrix: DMat): Vector[Double] =
    Vector.tabulate(matrix.rows * matrix.cols) { flat =>
      matrix(flat / matrix.cols, flat % matrix.cols)
    }

  private def hasHomogeneousBottomRow(
      values: Vector[Double],
      rank: Int,
      tolerance: Double
  ): Boolean =
    val size = rank + 1
    var column = 0
    var valid = true
    while column < rank && valid do
      valid = math.abs(values(rank * size + column)) <= tolerance
      column += 1
    valid && math.abs(values(rank * size + rank) - 1.0) <= tolerance

  private def canonicalizeHomogeneousBottomRow(
      values: Vector[Double],
      rank: Int
  ): Vector[Double] =
    val size = rank + 1
    values.patch(
      rank * size,
      Vector.fill(rank)(0.0) :+ 1.0,
      size
    )

/** Explicit name for image4s' equal-dimensional affine isomorphism.
  *
  * A future rectangular `AffineEmbedding[I, W]` will be a separate type and will not change the
  * meaning of this alias.
  */
type AffineIso[D <: Dim] = Affine[D]
