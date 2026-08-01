package image4s.ops

import image4s.geometry.Dim
import image4s.geometry.Dimension

/** Integer lattice offset in spatial index coordinates. */
final class Offset[D <: Dim] private (
    val coordinates: Vector[Int]
):
  def apply(axis: Int): Int =
    coordinates(axis)

  def rank(using dimension: Dimension[D]): Int =
    dimension.rank

object Offset:
  def create[D <: Dim](
      coordinates: IterableOnce[Int]
  )(using dimension: Dimension[D]): Either[OpError, Offset[D]] =
    val values = coordinates.iterator.toVector
    if values.length != dimension.rank then
      Left(
        OpError.InvalidOffset(
          s"offset rank ${values.length} does not match spatial rank ${dimension.rank}"
        )
      )
    else Right(new Offset(values))

  def zero[D <: Dim](using dimension: Dimension[D]): Offset[D] =
    new Offset(Vector.fill(dimension.rank)(0))

  def unsafe[D <: Dim](coordinates: Vector[Int]): Offset[D] =
    new Offset(coordinates)
