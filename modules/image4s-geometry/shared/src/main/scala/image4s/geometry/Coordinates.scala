package image4s.geometry

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

/** Coordinate values retain their exact spatial4s owner; these are aliases. */
type Point[F <: Frame[D], D <: Dim] = spatial4s.Point[F, D]
object Point:
  def in[D <: Dim](frame: Frame[D])(coordinates: Double*)(using
      Dimension[D]
  ): Either[GeometryError, Point[frame.type, D]] =
    fromVector(frame, coordinates.toVector)
  def fromVector[D <: Dim](frame: Frame[D], coordinates: Vector[Double])(using
      Dimension[D]
  ): Either[GeometryError, Point[frame.type, D]] =
    spatial4s.Point.fromVector(frame, coordinates).left.map(GeometryError.fromSpatial)
  private[geometry] def fromVectorAs[D <: Dim, F <: Frame[D]](
      frame: F,
      coordinates: Vector[Double]
  )(using Dimension[D]): Either[GeometryError, Point[F, D]] =
    for
      point <- fromVector(frame, coordinates)
      alignment <- Frame.alignOwners[D, frame.type, F](frame, frame)
      result <- alignment.pointToRight(point).left.map(GeometryError.fromSpatial)
    yield result

type Vec[F <: Frame[D], D <: Dim] = spatial4s.Vec[F, D]
object Vec:
  def in[D <: Dim](frame: Frame[D])(coordinates: Double*)(using
      Dimension[D]
  ): Either[GeometryError, Vec[frame.type, D]] =
    fromVector(frame, coordinates.toVector)
  def fromVector[D <: Dim](frame: Frame[D], coordinates: Vector[Double])(using
      Dimension[D]
  ): Either[GeometryError, Vec[frame.type, D]] =
    spatial4s.Vec.fromVector(frame, coordinates).left.map(GeometryError.fromSpatial)
  private[geometry] def fromVectorAs[D <: Dim, F <: Frame[D]](
      frame: F,
      coordinates: Vector[Double]
  )(using Dimension[D]): Either[GeometryError, Vec[F, D]] =
    for
      vector <- fromVector(frame, coordinates)
      alignment <- Frame.alignOwners[D, frame.type, F](frame, frame)
      result <- alignment.vectorToRight(vector).left.map(GeometryError.fromSpatial)
    yield result
