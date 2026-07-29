package image4s

import scala.annotation.tailrec

opaque type AxisName = String

object AxisName:
  def parse(value: String): Either[ImageError, AxisName] =
    val normalized = value.trim
    if normalized.nonEmpty && normalized == value then Right(value)
    else Left(ImageError.InvalidAxisName(value))

  extension (name: AxisName)
    def value: String = name

enum AxisKind derives CanEqual:
  case Time, Channel, Echo, Coil, Direction, Batch, Other

final class Axis private (
    val name: AxisName,
    val extent: Int,
    val kind: AxisKind
):
  override def toString: String =
    s"Axis(${name.value}, $extent, $kind)"

object Axis:
  def create(
      name: String,
      extent: Int,
      kind: AxisKind
  ): Either[ImageError, Axis] =
    AxisName.parse(name).flatMap { parsed =>
      if extent > 0 then Right(new Axis(parsed, extent, kind))
      else Left(ImageError.NonPositiveAxisExtent(name, extent))
    }

final class NonSpatialAxes private (
    val values: Vector[Axis]
):
  val shape: Vector[Int] =
    values.map(_.extent)

  def size: Int =
    values.size

  def apply(index: Int): Option[Axis] =
    values.lift(index)

  def append(axis: Axis): Either[ImageError, NonSpatialAxes] =
    NonSpatialAxes.from(values :+ axis)

  def remove(index: Int): Either[ImageError, NonSpatialAxes] =
    if index < 0 || index >= values.size then
      Left(ImageError.NonSpatialAxisOutOfBounds(index, values.size))
    else Right(new NonSpatialAxes(values.patch(index, Vector.empty, 1)))

  def updated(
      index: Int,
      axis: Axis
  ): Either[ImageError, NonSpatialAxes] =
    if index < 0 || index >= values.size then
      Left(ImageError.NonSpatialAxisOutOfBounds(index, values.size))
    else NonSpatialAxes.from(values.updated(index, axis))

  private[image4s] def without(index: Int): NonSpatialAxes =
    new NonSpatialAxes(values.patch(index, Vector.empty, 1))

object NonSpatialAxes:
  val empty: NonSpatialAxes =
    new NonSpatialAxes(Vector.empty)

  def from(
      axes: IterableOnce[Axis]
  ): Either[ImageError, NonSpatialAxes] =
    val copied = axes.iterator.toVector
    firstDuplicateName(copied.toList, Set.empty) match
      case Some(name) => Left(ImageError.DuplicateAxisName(name))
      case None       => Right(new NonSpatialAxes(copied))

  @tailrec
  private def firstDuplicateName(
      remaining: List[Axis],
      seen: Set[String]
  ): Option[String] =
    remaining match
      case head :: _ if seen.contains(head.name.value) =>
        Some(head.name.value)
      case head :: tail =>
        firstDuplicateName(tail, seen + head.name.value)
      case Nil =>
        None
