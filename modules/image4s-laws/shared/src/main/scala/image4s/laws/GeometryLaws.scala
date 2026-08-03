package image4s.laws

import image4s.geometry.Affine
import image4s.geometry.ContinuousIndex
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.FrameAlignment
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridAlignment
import image4s.geometry.Point
import image4s.geometry.Vec

/** Reusable laws for equal-dimensional affine image geometry. */
object GeometryLaws:
  def affineIdentity[D <: Dim](
      coordinates: Vector[Double],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    Affine
      .identity[D]
      .apply(coordinates)
      .map(closeVector(_, coordinates, tolerance))

  def affineInverseRoundTrip[D <: Dim](
      affine: Affine[D],
      coordinates: Vector[Double],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      transformed <- affine(coordinates)
      recovered <- affine.inverse(transformed)
    yield closeVector(recovered, coordinates, tolerance)

  def affineComposition[D <: Dim](
      first: Affine[D],
      second: Affine[D],
      coordinates: Vector[Double],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      firstValue <- first(coordinates)
      sequential <- second(firstValue)
      composed <- first.andThen(second)
      direct <- composed(coordinates)
    yield closeVector(sequential, direct, tolerance)

  def affineCompositionAssociative[D <: Dim](
      first: Affine[D],
      second: Affine[D],
      third: Affine[D],
      coordinates: Vector[Double],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      firstThenSecond <- first.andThen(second)
      leftAssociated <- firstThenSecond.andThen(third)
      secondThenThird <- second.andThen(third)
      rightAssociated <- first.andThen(secondThenThird)
      leftValue <- leftAssociated(coordinates)
      rightValue <- rightAssociated(coordinates)
    yield closeVector(leftValue, rightValue, tolerance)

  def pointTranslationRoundTrip[
      F <: Frame[D],
      D <: Dim
  ](
      point: Point[F, D],
      vector: Vec[F, D],
      tolerance: Double
  ): Either[GeometryError, Boolean] =
    val moved = point + vector
    val recovered = moved - point
    Right(
      closeVector(
        recovered.coordinates,
        vector.coordinates,
        tolerance
      )
    )

  def pointTranslationComposition[
      F <: Frame[D],
      D <: Dim
  ](
      point: Point[F, D],
      first: Vec[F, D],
      second: Vec[F, D],
      tolerance: Double
  ): Boolean =
    val sequential = (point + first) + second
    val combined = point + (first + second)
    closeVector(
      sequential.coordinates,
      combined.coordinates,
      tolerance
    )

  def gridContinuousRoundTrip[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      index: ContinuousIndex[D],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      point <- grid.pointAt(index)
      recovered <- grid.continuousIndexOf(point)
    yield closeVector(recovered.values, index.values, tolerance)

  def frameAlignmentRoundTrip[
      D <: Dim,
      LF <: Frame[D],
      RF <: Frame[D]
  ](
      alignment: FrameAlignment[D, LF, RF],
      point: Point[LF, D],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      right <- alignment.pointToRight(point)
      recovered <- alignment.pointToLeft(right)
    yield closeVector(
      recovered.coordinates,
      point.coordinates,
      tolerance
    )

  def gridAlignmentRoundTrip[
      D <: Dim,
      LF <: Frame[D],
      RF <: Frame[D]
  ](
      alignment: GridAlignment[D, LF, RF],
      point: Point[LF, D],
      tolerance: Double
  )(using Dimension[D]): Either[GeometryError, Boolean] =
    for
      right <- alignment.pointToRight(point)
      recovered <- alignment.pointToLeft(right)
    yield closeVector(
      recovered.coordinates,
      point.coordinates,
      tolerance
    )

  private def closeVector(
      left: Vector[Double],
      right: Vector[Double],
      tolerance: Double
  ): Boolean =
    tolerance.isFinite &&
      tolerance >= 0.0 &&
      left.size == right.size &&
      left.zip(right).forall { case (a, b) =>
        math.abs(a - b) <= tolerance
      }
