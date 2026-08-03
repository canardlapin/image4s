package image4s.ops

import scala.annotation.unused
import image4s.geometry.Dim
import image4s.geometry.Dimension

/** Finite, duplicate-free, canonically ordered neighborhood support.
  *
  * Independent of weights and element type. Shared by linear filters, rank filters, morphology, and
  * connectivity definitions.
  */
final class Support[D <: Dim] private (
    val offsets: Vector[Offset[D]]
):
  def size: Int =
    offsets.length

  def isEmpty: Boolean =
    offsets.isEmpty

  def nonEmpty: Boolean =
    offsets.nonEmpty

  /** Inclusive left footprint extents (max non-positive displacement magnitude). */
  def leftExtents(using dimension: Dimension[D]): Vector[Int] =
    val rank = dimension.rank
    val left = Array.fill(rank)(0)
    var i = 0
    while i < offsets.length do
      val coords = offsets(i).coordinates
      var axis = 0
      while axis < rank do
        val extent = -coords(axis)
        if extent > left(axis) then left(axis) = extent
        axis += 1
      i += 1
    left.toVector

  /** Inclusive right footprint extents (max non-negative displacement). */
  def rightExtents(using dimension: Dimension[D]): Vector[Int] =
    val rank = dimension.rank
    val right = Array.fill(rank)(0)
    var i = 0
    while i < offsets.length do
      val coords = offsets(i).coordinates
      var axis = 0
      while axis < rank do
        val extent = coords(axis)
        if extent > right(axis) then right(axis) = extent
        axis += 1
      i += 1
    right.toVector

object Support:
  def create[D <: Dim](
      offsets: IterableOnce[Offset[D]]
  )(using @unused dimension: Dimension[D]): Either[OpError, Support[D]] =
    val raw = offsets.iterator.toVector
    if raw.isEmpty then Left(OpError.InvalidSupport("support must be non-empty"))
    else
      val sorted =
        raw.sortWith { (left, right) =>
          compareCoordinates(left.coordinates, right.coordinates) < 0
        }
      var index = 1
      var duplicate: Option[OpError] = None
      while index < sorted.length && duplicate.isEmpty do
        if sameCoordinates(sorted(index - 1), sorted(index)) then
          duplicate = Some(
            OpError.InvalidSupport(
              s"duplicate offset ${sorted(index).coordinates.mkString("(", ",", ")")}"
            )
          )
        index += 1
      duplicate match
        case Some(err) => Left(err)
        case None => Right(new Support(sorted))

  def singleton[D <: Dim](
      offset: Offset[D]
  ): Support[D] =
    new Support(Vector(offset))

  def origin[D <: Dim](using dimension: Dimension[D]): Support[D] =
    singleton(Offset.zero)

  private def sameCoordinates[D <: Dim](
      left: Offset[D],
      right: Offset[D]
  ): Boolean =
    compareCoordinates(left.coordinates, right.coordinates) == 0

  private def compareCoordinates(
      left: Vector[Int],
      right: Vector[Int]
  ): Int =
    var axis = 0
    val n = left.length.min(right.length)
    while axis < n do
      val cmp = Integer.compare(left(axis), right(axis))
      if cmp != 0 then return cmp
      axis += 1
    Integer.compare(left.length, right.length)
