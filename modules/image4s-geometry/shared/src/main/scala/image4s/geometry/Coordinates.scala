package image4s.geometry

import gale.linalg.DVec

final class Index[D <: Dim] private (val values: Vector[Int]):
  def apply(axis: Int): Option[Int] =
    values.lift(axis)

object Index:
  def of[D <: Dim](
      values: Int*
  )(using dimension: Dimension[D]): Either[GeometryError, Index[D]] =
    fromVector(values.toVector)

  def fromVector[D <: Dim](
      values: Vector[Int]
  )(using dimension: Dimension[D]): Either[GeometryError, Index[D]] =
    if values.length == dimension.rank then Right(new Index(values))
    else Left(GeometryError.DimensionMismatch(dimension.rank, values.length))

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

  def +(vector: Vec[F, D]): Either[GeometryError, Point[F, D]] =
    if !vector.belongsTo(frame) then
      Left(frame.mismatchWith(vector.frame))
    else
      Right(
        new Point(
          frame,
          DVec.tabulate(values.length)(axis =>
            values(axis) + vector.coordinateUnsafe(axis)
          )
        )
      )

  def -(other: Point[F, D])(using
      Dimension[D]
  ): Either[GeometryError, Vec[F, D]] =
    if !other.belongsTo(frame) then
      Left(frame.mismatchWith(other.frame))
    else
      Vec.fromVector(
        frame,
        Vector.tabulate(values.length)(axis =>
            values(axis) - other.coordinateUnsafe(axis)
        )
      )

  private[geometry] def coordinateUnsafe(axis: Int): Double =
    values(axis)

object Point:
  def in[D <: Dim](
      frame: Frame[D]
  )(coordinates: Double*)(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[frame.type, D]] =
    fromVector[D, frame.type](frame, coordinates.toVector)

  def fromVector[D <: Dim, F <: Frame[D]](
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

  def +(other: Vec[F, D]): Either[GeometryError, Vec[F, D]] =
    if !other.belongsTo(frame) then
      Left(frame.mismatchWith(other.frame))
    else
      Right(
        new Vec(
          frame,
          DVec.tabulate(values.length)(axis =>
            values(axis) + other.coordinateUnsafe(axis)
          )
        )
      )

  private[geometry] def coordinateUnsafe(axis: Int): Double =
    values(axis)

object Vec:
  def in[D <: Dim](
      frame: Frame[D]
  )(coordinates: Double*)(using
      dimension: Dimension[D]
  ): Either[GeometryError, Vec[frame.type, D]] =
    Coordinates.validate[D](coordinates.toVector).map(values =>
      new Vec(frame, DVec.tabulate(dimension.rank)(values))
    )

  def fromVector[D <: Dim, F <: Frame[D]](
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
