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

opaque type AxisKindId = String

object AxisKindId:
  private[image4s] def parse(value: String): Either[ImageError, AxisKindId] =
    if AxisIdentifier.isValid(value) then Right(value)
    else Left(ImageError.InvalidAxisKindId(value))

  extension (id: AxisKindId)
    def value: String = id

/** Semantic category of a non-spatial sampling axis.
  *
  * Known kinds support concise common APIs. `Custom` lets downstream domains
  * retain their own stable meaning without extending image4s.
  */
enum AxisKind derives CanEqual:
  case Time, Channel, Echo, Coil, Direction, Batch, Other
  case Custom(customId: AxisKindId)

  def id: String =
    this match
      case Time       => "time"
      case Channel    => "channel"
      case Echo       => "echo"
      case Coil       => "coil"
      case Direction  => "direction"
      case Batch      => "batch"
      case Other      => "other"
      case Custom(customId) => customId.value

object AxisKind:
  private val known: Vector[AxisKind] =
    Vector(Time, Channel, Echo, Coil, Direction, Batch, Other)

  def fromId(value: String): Either[ImageError, AxisKind] =
    known.find(_.id == value) match
      case Some(kind) => Right(kind)
      case None       => custom(value)

  def custom(value: String): Either[ImageError, AxisKind] =
    AxisKindId.parse(value).flatMap { id =>
      if known.exists(_.id == id.value) then
        Left(ImageError.ReservedAxisKindId(value))
      else Right(Custom(id))
    }

opaque type AxisUnitId = String

object AxisUnitId:
  private[image4s] def parse(value: String): Either[ImageError, AxisUnitId] =
    if AxisIdentifier.isValid(value) then Right(value)
    else Left(ImageError.InvalidAxisUnitId(value))

  extension (id: AxisUnitId)
    def value: String = id

/** Unit attached to numeric non-spatial coordinates.
  *
  * This is an identifier vocabulary rather than a units-of-measure system.
  * Conversion and dimensional compatibility remain explicit operations.
  */
enum AxisUnit derives CanEqual:
  case Unitless
  case Seconds
  case Milliseconds
  case Microseconds
  case Hertz
  case PartsPerMillion
  case RadiansPerSecond
  case Degrees
  case Radians
  case Custom(customId: AxisUnitId)

  def id: String =
    this match
      case Unitless        => "unitless"
      case Seconds         => "s"
      case Milliseconds    => "ms"
      case Microseconds    => "us"
      case Hertz           => "hz"
      case PartsPerMillion => "ppm"
      case RadiansPerSecond =>
        "rad/s"
      case Degrees    => "degree"
      case Radians    => "radian"
      case Custom(customId) => customId.value

object AxisUnit:
  private val known: Vector[AxisUnit] =
    Vector(
      Unitless,
      Seconds,
      Milliseconds,
      Microseconds,
      Hertz,
      PartsPerMillion,
      RadiansPerSecond,
      Degrees,
      Radians
    )

  def fromId(value: String): Either[ImageError, AxisUnit] =
    known.find(_.id == value) match
      case Some(unit) => Right(unit)
      case None       => custom(value)

  def custom(value: String): Either[ImageError, AxisUnit] =
    AxisUnitId.parse(value).flatMap { id =>
      if known.exists(_.id == id.value) then
        Left(ImageError.ReservedAxisUnitId(value))
      else Right(Custom(id))
    }

/** Coordinate returned by a checked lookup on an axis. */
enum AxisCoordinate derives CanEqual:
  case Ordinal(index: Int)
  case Numeric(value: Double, unit: AxisUnit)
  case Categorical(label: String)

/** Neutral, untrusted serialization record for axis coordinates.
  *
  * Record constructors do not validate external input. Use
  * [[Axis.fromRecord]] to recover a checked live axis.
  */
enum AxisCoordinatesRecord derives CanEqual:
  case Ordinal(extent: Int)
  case Regular(
      extent: Int,
      origin: Double,
      step: Double,
      unit: String
  )
  case Explicit(
      values: Vector[Double],
      unit: String
  )
  case Categorical(labels: Vector[String])

/** Neutral structural record of one non-spatial sampling axis.
  *
  * This record describes sampling; it is not a persistent identity key.
  */
final case class AxisRecord(
    name: String,
    kind: String,
    coordinates: AxisCoordinatesRecord
) derives CanEqual

private enum ValidatedAxisCoordinates:
  case Ordinal(extent: Int)
  case Regular(
      extent: Int,
      origin: Double,
      step: Double,
      unit: AxisUnit
  )
  case Explicit(
      values: Vector[Double],
      unit: AxisUnit
  )
  case Categorical(labels: Vector[String])

/** Validated finite coordinates for a non-spatial axis.
  *
  * Structural equality is intentional for this small immutable descriptor.
  * A live [[Axis]] retains reference identity.
  */
final class AxisCoordinates private (
    private val validated: ValidatedAxisCoordinates
):
  val record: AxisCoordinatesRecord =
    validated match
      case ValidatedAxisCoordinates.Ordinal(extent) =>
        AxisCoordinatesRecord.Ordinal(extent)
      case ValidatedAxisCoordinates.Regular(
            extent,
            origin,
            step,
            unit
          ) =>
        AxisCoordinatesRecord.Regular(
          extent,
          origin,
          step,
          unit.id
        )
      case ValidatedAxisCoordinates.Explicit(values, unit) =>
        AxisCoordinatesRecord.Explicit(values, unit.id)
      case ValidatedAxisCoordinates.Categorical(labels) =>
        AxisCoordinatesRecord.Categorical(labels)

  val extent: Int =
    validated match
      case ValidatedAxisCoordinates.Ordinal(value) =>
        value
      case ValidatedAxisCoordinates.Regular(value, _, _, _) =>
        value
      case ValidatedAxisCoordinates.Explicit(values, _) =>
        values.size
      case ValidatedAxisCoordinates.Categorical(labels) =>
        labels.size

  def apply(index: Int): Option[AxisCoordinate] =
    Option.when(index >= 0 && index < extent)(coordinateAtUnchecked(index))

  override def equals(other: Any): Boolean =
    other match
      case that: AxisCoordinates =>
        (this eq that) || record == that.record
      case _ =>
        false

  override def hashCode(): Int =
    record.hashCode()

  override def toString: String =
    record.toString

  private def coordinateAtUnchecked(index: Int): AxisCoordinate =
    validated match
      case ValidatedAxisCoordinates.Ordinal(_) =>
        AxisCoordinate.Ordinal(index)
      case ValidatedAxisCoordinates.Regular(_, origin, step, unit) =>
        AxisCoordinate.Numeric(
          origin + index.toDouble * step,
          unit
        )
      case ValidatedAxisCoordinates.Explicit(values, unit) =>
        AxisCoordinate.Numeric(
          values(index),
          unit
        )
      case ValidatedAxisCoordinates.Categorical(labels) =>
        AxisCoordinate.Categorical(labels(index))

object AxisCoordinates:
  private[image4s] def fromRecord(
      axisName: String,
      record: AxisCoordinatesRecord
  ): Either[ImageError, AxisCoordinates] =
    record match
      case AxisCoordinatesRecord.Ordinal(extent) =>
        positiveExtent(axisName, extent).map(_ =>
          new AxisCoordinates(ValidatedAxisCoordinates.Ordinal(extent))
        )
      case AxisCoordinatesRecord.Regular(extent, origin, step, unitId) =>
        for
          _ <- positiveExtent(axisName, extent)
          _ <-
            Either.cond(
              origin.isFinite,
              (),
              ImageError.NonFiniteAxisOrigin(axisName, origin)
            )
          _ <-
            Either.cond(
              step.isFinite && step != 0.0,
              (),
              ImageError.InvalidAxisStep(axisName, step)
            )
          unit <- AxisUnit.fromId(unitId)
        yield new AxisCoordinates(
          ValidatedAxisCoordinates.Regular(
            extent,
            origin,
            step,
            unit
          )
        )
      case AxisCoordinatesRecord.Explicit(values, unitId) =>
        for
          _ <- positiveExtent(axisName, values.size)
          _ <- values.zipWithIndex.collectFirst {
            case (value, index) if !value.isFinite =>
              ImageError.NonFiniteAxisCoordinate(axisName, index, value)
          }.toLeft(())
          unit <- AxisUnit.fromId(unitId)
        yield new AxisCoordinates(
          ValidatedAxisCoordinates.Explicit(values, unit)
        )
      case AxisCoordinatesRecord.Categorical(labels) =>
        for
          _ <- positiveExtent(axisName, labels.size)
          _ <- labels.zipWithIndex.collectFirst {
            case (label, index) if !AxisLabel.isValid(label) =>
              ImageError.InvalidCategoricalAxisLabel(axisName, index, label)
          }.toLeft(())
        yield new AxisCoordinates(
          ValidatedAxisCoordinates.Categorical(labels)
        )

  private def positiveExtent(
      axisName: String,
      extent: Int
  ): Either[ImageError, Unit] =
    Either.cond(
      extent > 0,
      (),
      ImageError.NonPositiveAxisExtent(axisName, extent)
    )

final class Axis private (
    val name: AxisName,
    val kind: AxisKind,
    val coordinates: AxisCoordinates
):
  val extent: Int =
    coordinates.extent

  val record: AxisRecord =
    AxisRecord(name.value, kind.id, coordinates.record)

  def coordinateAt(index: Int): Either[ImageError, AxisCoordinate] =
    coordinates(index).toRight(
      ImageError.NonSpatialIndexOutOfBounds(name, index, extent)
    )

  override def toString: String =
    s"Axis(${name.value}, $kind, $coordinates)"

object Axis:
  /** Construct an ordinal axis.
    *
    * This compatibility constructor is equivalent to [[ordinal]].
    */
  def create(
      name: String,
      extent: Int,
      kind: AxisKind
  ): Either[ImageError, Axis] =
    ordinal(name, kind, extent)

  def ordinal(
      name: String,
      kind: AxisKind,
      extent: Int
  ): Either[ImageError, Axis] =
    checked(
      name,
      kind,
      AxisCoordinatesRecord.Ordinal(extent)
    )

  def regular(
      name: String,
      kind: AxisKind,
      extent: Int,
      origin: Double,
      step: Double,
      unit: AxisUnit
  ): Either[ImageError, Axis] =
    checked(
      name,
      kind,
      AxisCoordinatesRecord.Regular(
        extent,
        origin,
        step,
        unit.id
      )
    )

  def explicit(
      name: String,
      kind: AxisKind,
      values: IterableOnce[Double],
      unit: AxisUnit
  ): Either[ImageError, Axis] =
    checked(
      name,
      kind,
      AxisCoordinatesRecord.Explicit(
        values.iterator.toVector,
        unit.id
      )
    )

  def categorical(
      name: String,
      kind: AxisKind,
      labels: IterableOnce[String]
  ): Either[ImageError, Axis] =
    checked(
      name,
      kind,
      AxisCoordinatesRecord.Categorical(labels.iterator.toVector)
    )

  def fromRecord(record: AxisRecord): Either[ImageError, Axis] =
    for
      name <- AxisName.parse(record.name)
      kind <- AxisKind.fromId(record.kind)
      coordinates <- AxisCoordinates.fromRecord(
        record.name,
        record.coordinates
      )
    yield new Axis(name, kind, coordinates)

  private def checked(
      rawName: String,
      kind: AxisKind,
      record: AxisCoordinatesRecord
  ): Either[ImageError, Axis] =
    for
      name <- AxisName.parse(rawName)
      coordinates <- AxisCoordinates.fromRecord(rawName, record)
    yield new Axis(name, kind, coordinates)

final class NonSpatialAxes private (
    val values: Vector[Axis]
):
  val shape: Vector[Int] =
    values.map(_.extent)

  val records: Vector[AxisRecord] =
    values.map(_.record)

  def size: Int =
    values.size

  def apply(index: Int): Option[Axis] =
    values.lift(index)

  def coordinateAt(
      axis: Int,
      index: Int
  ): Either[ImageError, AxisCoordinate] =
    apply(axis)
      .toRight(ImageError.NonSpatialAxisOutOfBounds(axis, size))
      .flatMap(_.coordinateAt(index))

  def uniqueIndexOf(kind: AxisKind): Either[ImageError, Int] =
    var found = -1
    var count = 0
    var axis = 0
    while axis < values.length do
      if values(axis).kind == kind then
        found = axis
        count += 1
      axis += 1
    if count == 0 then Left(ImageError.MissingNonSpatialAxisKind(kind))
    else if count > 1 then
      Left(ImageError.AmbiguousNonSpatialAxisKind(kind, count))
    else Right(found)

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

  def permute(
      order: IterableOnce[Int]
  ): Either[ImageError, NonSpatialAxes] =
    val copied = order.iterator.toVector
    if copied.size != size then
      Left(
        ImageError.NonSpatialAxisPermutationRankMismatch(
          size,
          copied.size
        )
      )
    else if copied.sorted != values.indices.toVector then
      Left(ImageError.InvalidNonSpatialAxisPermutation(copied, size))
    else
      Right(new NonSpatialAxes(copied.map(values)))

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

  def fromRecords(
      records: IterableOnce[AxisRecord]
  ): Either[ImageError, NonSpatialAxes] =
    records.iterator.foldLeft[
      Either[ImageError, Vector[Axis]]
    ](Right(Vector.empty)) { (accumulated, record) =>
      for
        axes <- accumulated
        axis <- Axis.fromRecord(record)
      yield axes :+ axis
    }.flatMap(from)

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

private object AxisIdentifier:
  def isValid(value: String): Boolean =
    value.nonEmpty &&
      value == value.trim &&
      isLowerAsciiLetter(value.head) &&
      value.forall(isAllowed)

  private def isAllowed(value: Char): Boolean =
    isLowerAsciiLetter(value) ||
      (value >= '0' && value <= '9') ||
      value == '-' ||
      value == '_' ||
      value == '.' ||
      value == ':' ||
      value == '/'

  private def isLowerAsciiLetter(value: Char): Boolean =
    value >= 'a' && value <= 'z'

private object AxisLabel:
  def isValid(value: String): Boolean =
    value.nonEmpty && value == value.trim
