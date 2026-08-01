package image4s.geometry

import gale.linalg.DVec

/** Integer coordinate in the unbounded lattice underlying a grid. */
final class LatticeIndex[D <: Dim] private (
    val values: Vector[Int]
):
  def apply(axis: Int): Option[Int] =
    values.lift(axis)

object LatticeIndex:
  def of[D <: Dim](
      values: Int*
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, LatticeIndex[D]] =
    fromVector(values.toVector)

  def fromVector[D <: Dim](
      values: Vector[Int]
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, LatticeIndex[D]] =
    if values.length == dimension.rank then Right(new LatticeIndex(values))
    else Left(GeometryError.DimensionMismatch(dimension.rank, values.length))

/** Migration alias. New code should state the unbounded lattice semantics. */
@deprecated("Use LatticeIndex", "0.2.0")
type Index[D <: Dim] = LatticeIndex[D]

@deprecated("Use LatticeIndex", "0.2.0")
object Index:
  def of[D <: Dim](
      values: Int*
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, LatticeIndex[D]] =
    LatticeIndex.fromVector(values.toVector)

  def fromVector[D <: Dim](
      values: Vector[Int]
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, LatticeIndex[D]] =
    LatticeIndex.fromVector(values)

/** Floating-point coordinate in an unbounded continuous index space. */
final class ContinuousIndex[D <: Dim] private (
    val values: Vector[Double]
):
  def apply(axis: Int): Option[Double] =
    values.lift(axis)

object ContinuousIndex:
  def of[D <: Dim](
      values: Double*
  )(using dimension: Dimension[D]): Either[GeometryError, ContinuousIndex[D]] =
    fromVector(values.toVector)

  def fromVector[D <: Dim](
      values: Vector[Double]
  )(using dimension: Dimension[D]): Either[GeometryError, ContinuousIndex[D]] =
    if values.length != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, values.length))
    else if values.exists(value => !value.isFinite) then
      val axis = values.indexWhere(value => !value.isFinite)
      Left(GeometryError.NonFiniteCoordinate(axis, values(axis)))
    else Right(new ContinuousIndex(values))

final class Point[F <: Frame[D], D <: Dim] private (
    val frame: F,
    private val values: DVec
):
  def coordinate(axis: Int): Option[Double] =
    if axis >= 0 && axis < values.length then Some(values(axis)) else None

  def coordinates: Vector[Double] =
    values.toSeq.toVector

  def belongsTo(other: Frame[?]): Boolean =
    frame.sameRuntimeOwnerAs(other)

  /** Translate by a vector with the same static frame owner. */
  def +(vector: Vec[F, D]): Point[F, D] =
    new Point(
      frame,
      DVec.tabulate(values.length)(axis =>
        values(axis) + vector.coordinateUnsafe(axis)
      )
    )

  /** Translate after runtime owner information has been erased. */
  def addChecked[OtherF <: Frame[D]](
      vector: Vec[OtherF, D]
  ): Either[GeometryError, Point[F, D]] =
    if vector.belongsTo(frame) then
      Right(
        new Point(
          frame,
          DVec.tabulate(values.length)(axis =>
            values(axis) + vector.coordinateUnsafe(axis)
          )
        )
      )
    else Left(frame.mismatchWith(vector.frame))

  /** Subtract a point with the same static frame owner. */
  def -(other: Point[F, D]): Vec[F, D] =
    Vec.fromDVecAs(
      frame,
      DVec.tabulate(values.length)(axis =>
        values(axis) - other.coordinateUnsafe(axis)
      )
    )

  /** Subtract after runtime owner information has been erased. */
  def subtractChecked[OtherF <: Frame[D]](
      other: Point[OtherF, D]
  ): Either[GeometryError, Vec[F, D]] =
    if other.belongsTo(frame) then Right(this - reowned(other))
    else Left(frame.mismatchWith(other.frame))

  private def reowned[OtherF <: Frame[D]](
      other: Point[OtherF, D]
  ): Point[F, D] =
    new Point(frame, other.values)

  private[geometry] def coordinateUnsafe(axis: Int): Double =
    values(axis)

object Point:
  def in[D <: Dim](
      frame: Frame[D]
  )(coordinates: Double*)(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[frame.type, D]] =
    fromVector(frame, coordinates.toVector)

  def fromVector[D <: Dim](
      frame: Frame[D],
      coordinates: Vector[Double]
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[frame.type, D]] =
    Coordinates.validate[D](coordinates).map(values =>
      new Point(frame, DVec.tabulate(dimension.rank)(values))
    )

  private[geometry] def fromVectorAs[D <: Dim, F <: Frame[D]](
      frame: F,
      coordinates: Vector[Double]
  )(using dimension: Dimension[D]): Either[GeometryError, Point[F, D]] =
    Coordinates.validate[D](coordinates).map(values =>
      new Point(frame, DVec.tabulate(dimension.rank)(values))
    )

final class Vec[F <: Frame[D], D <: Dim] private (
    val frame: F,
    private val values: DVec
):
  def coordinate(axis: Int): Option[Double] =
    if axis >= 0 && axis < values.length then Some(values(axis)) else None

  def coordinates: Vector[Double] =
    values.toSeq.toVector

  def belongsTo(other: Frame[?]): Boolean =
    frame.sameRuntimeOwnerAs(other)

  /** Add a vector with the same static frame owner. */
  def +(other: Vec[F, D]): Vec[F, D] =
    new Vec(
      frame,
      DVec.tabulate(values.length)(axis =>
        values(axis) + other.coordinateUnsafe(axis)
      )
    )

  /** Add after runtime owner information has been erased. */
  def addChecked[OtherF <: Frame[D]](
      other: Vec[OtherF, D]
  ): Either[GeometryError, Vec[F, D]] =
    if other.belongsTo(frame) then
      Right(
        new Vec(
          frame,
          DVec.tabulate(values.length)(axis =>
            values(axis) + other.coordinateUnsafe(axis)
          )
        )
      )
    else Left(frame.mismatchWith(other.frame))

  private[geometry] def coordinateUnsafe(axis: Int): Double =
    values(axis)

object Vec:
  private[geometry] def fromDVecAs[D <: Dim, F <: Frame[D]](
      frame: F,
      values: DVec
  ): Vec[F, D] =
    new Vec(frame, values)

  def in[D <: Dim](
      frame: Frame[D]
  )(coordinates: Double*)(using
      dimension: Dimension[D]
  ): Either[GeometryError, Vec[frame.type, D]] =
    fromVector(frame, coordinates.toVector)

  def fromVector[D <: Dim](
      frame: Frame[D],
      coordinates: Vector[Double]
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, Vec[frame.type, D]] =
    Coordinates.validate[D](coordinates).map(values =>
      new Vec(frame, DVec.tabulate(dimension.rank)(values))
    )

  private[geometry] def fromVectorAs[D <: Dim, F <: Frame[D]](
      frame: F,
      coordinates: Vector[Double]
  )(using dimension: Dimension[D]): Either[GeometryError, Vec[F, D]] =
    Coordinates.validate[D](coordinates).map(values =>
      new Vec(frame, DVec.tabulate(dimension.rank)(values))
    )

private object Coordinates:
  def validate[D <: Dim](
      coordinates: Vector[Double]
  )(using dimension: Dimension[D]): Either[GeometryError, Vector[Double]] =
    if coordinates.length != dimension.rank then
      Left(
        GeometryError.DimensionMismatch(dimension.rank, coordinates.length)
      )
    else if coordinates.exists(value => !value.isFinite) then
      val axis = coordinates.indexWhere(value => !value.isFinite)
      Left(GeometryError.NonFiniteCoordinate(axis, coordinates(axis)))
    else Right(coordinates)
