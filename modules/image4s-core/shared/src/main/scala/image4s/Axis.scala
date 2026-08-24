package image4s

import scala.annotation.tailrec

opaque type AxisName = String

object AxisName:
  def parse(value: String): Either[ImageError, AxisName] =
    val normalized = value.trim
    if normalized.nonEmpty && normalized == value then Right(value)
    else Left(ImageError.InvalidAxisName(value))

  extension (name: AxisName) def value: String = name

opaque type AxisKindId = String

object AxisKindId:
  private[image4s] def parse(value: String): Either[ImageError, AxisKindId] =
    if AxisIdentifier.isValid(value) then Right(value)
    else Left(ImageError.InvalidAxisKindId(value))

  extension (id: AxisKindId) def value: String = id

/** Semantic category of a non-spatial sampling axis.
  *
  * Known kinds support concise common APIs. `Custom` lets downstream domains retain their own
  * stable meaning without extending image4s.
  */
enum AxisKind derives CanEqual:
  case Time, Channel, Echo, Coil, Direction, Batch, Other
  case Custom(customId: AxisKindId)

  def id: String =
    this match
      case Time => "time"
      case Channel => "channel"
      case Echo => "echo"
      case Coil => "coil"
      case Direction => "direction"
      case Batch => "batch"
      case Other => "other"
      case Custom(customId) => customId.value

object AxisKind:
  private val known: Vector[AxisKind] =
    Vector(Time, Channel, Echo, Coil, Direction, Batch, Other)

  def fromId(value: String): Either[ImageError, AxisKind] =
    known.find(_.id == value) match
      case Some(kind) => Right(kind)
      case None => custom(value)

  def custom(value: String): Either[ImageError, AxisKind] =
    AxisKindId.parse(value).flatMap { id =>
      if known.exists(_.id == id.value) then Left(ImageError.ReservedAxisKindId(value))
      else Right(Custom(id))
    }

opaque type AxisUnitId = String

object AxisUnitId:
  private[image4s] def parse(value: String): Either[ImageError, AxisUnitId] =
    if AxisIdentifier.isValid(value) then Right(value)
    else Left(ImageError.InvalidAxisUnitId(value))

  extension (id: AxisUnitId) def value: String = id

/** Unit attached to numeric non-spatial coordinates.
  *
  * This is an identifier vocabulary rather than a units-of-measure system. Conversion and
  * dimensional compatibility remain explicit operations.
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
      case Unitless => "unitless"
      case Seconds => "s"
      case Milliseconds => "ms"
      case Microseconds => "us"
      case Hertz => "hz"
      case PartsPerMillion => "ppm"
      case RadiansPerSecond =>
        "rad/s"
      case Degrees => "degree"
      case Radians => "radian"
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
      case None => custom(value)

  def custom(value: String): Either[ImageError, AxisUnit] =
    AxisUnitId.parse(value).flatMap { id =>
      if known.exists(_.id == id.value) then Left(ImageError.ReservedAxisUnitId(value))
      else Right(Custom(id))
    }

/** Coordinate returned by a checked lookup on an axis. */
enum AxisCoordinate derives CanEqual:
  case Ordinal(index: Int)
  case Numeric(value: Double, unit: AxisUnit)
  case Categorical(label: String)

/** Neutral, untrusted serialization record for axis coordinates.
  *
  * Record constructors do not validate external input. Use [[Axis.fromRecord]] to recover a checked
  * live axis.
  */
enum AxisCoordinatesRecord derives CanEqual:
  case Ordinal(extent: Int)
  case OrdinalValues(values: Vector[Int])
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
  case OrdinalValues(values: Vector[Int])
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
  * Structural equality is intentional for this small immutable descriptor. A live [[Axis]] retains
  * reference identity.
  */
final class AxisCoordinates private (
    private val validated: ValidatedAxisCoordinates
):
  val record: AxisCoordinatesRecord =
    validated match
      case ValidatedAxisCoordinates.Ordinal(extent) =>
        AxisCoordinatesRecord.Ordinal(extent)
      case ValidatedAxisCoordinates.OrdinalValues(values) =>
        AxisCoordinatesRecord.OrdinalValues(values)
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
      case ValidatedAxisCoordinates.OrdinalValues(values) =>
        values.size
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
      case ValidatedAxisCoordinates.OrdinalValues(values) =>
        AxisCoordinate.Ordinal(values(index))
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
      case AxisCoordinatesRecord.OrdinalValues(values) =>
        for
          _ <- positiveExtent(axisName, values.size)
          _ <- values.zipWithIndex
            .collectFirst {
              case (value, index) if value < 0 =>
                ImageError.InvalidOrdinalAxisCoordinate(axisName, index, value)
            }
            .toLeft(())
        yield new AxisCoordinates(ValidatedAxisCoordinates.OrdinalValues(values))
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
          _ <- values.zipWithIndex
            .collectFirst {
              case (value, index) if !value.isFinite =>
                ImageError.NonFiniteAxisCoordinate(axisName, index, value)
            }
            .toLeft(())
          unit <- AxisUnit.fromId(unitId)
        yield new AxisCoordinates(
          ValidatedAxisCoordinates.Explicit(values, unit)
        )
      case AxisCoordinatesRecord.Categorical(labels) =>
        for
          _ <- positiveExtent(axisName, labels.size)
          _ <- labels.zipWithIndex
            .collectFirst {
              case (label, index) if !AxisLabel.isValid(label) =>
                ImageError.InvalidCategoricalAxisLabel(axisName, index, label)
            }
            .toLeft(())
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

  /** Select coordinates in the caller's declared order.
    *
    * Reversed, sparse, and duplicate indices are retained exactly. Selection is non-empty because
    * every validated [[Axis]] has positive extent.
    */
  def select(indices: IterableOnce[Int]): Either[ImageError, Axis] =
    val copied = indices.iterator.toVector
    copied.headOption
      .toRight(ImageError.EmptyAxisSelection(name))
      .flatMap { _ =>
        copied
          .find(index => index < 0 || index >= extent)
          .map(index => ImageError.NonSpatialIndexOutOfBounds(name, index, extent))
          .toLeft(())
      }
      .flatMap { _ =>
        if copied == Vector.range(0, extent) then Right(this)
        else
          val selectedRecord =
            record.coordinates match
              case AxisCoordinatesRecord.Ordinal(_) =>
                Axis.selectedOrdinalRecord(copied)
              case AxisCoordinatesRecord.OrdinalValues(values) =>
                Axis.selectedOrdinalRecord(copied.map(values))
              case AxisCoordinatesRecord.Regular(_, origin, step, unit) =>
                Axis.selectedRegularRecord(copied, origin, step, unit)
              case AxisCoordinatesRecord.Explicit(values, unit) =>
                AxisCoordinatesRecord.Explicit(copied.map(values), unit)
              case AxisCoordinatesRecord.Categorical(labels) =>
                AxisCoordinatesRecord.Categorical(copied.map(labels))
          Axis.checked(name.value, kind, selectedRecord)
      }

  /** Concatenate this axis with `other` under an explicit coordinate policy. */
  def concatenate(
      other: Axis,
      policy: AxisConcatenationPolicy
  ): Either[ImageError, Axis] =
    Axis.concatenate(this, other, policy)

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

  /** Concatenate two axes without reconstructing their sampling metadata downstream.
    *
    * Name and semantic kind must always match. Numeric coordinates additionally require identical
    * units. The policy decides whether declared coordinate order is sufficient or a continuous
    * regular boundary is required.
    */
  def concatenate(
      left: Axis,
      right: Axis,
      policy: AxisConcatenationPolicy
  ): Either[ImageError, Axis] =
    for
      _ <- compatibleName(left, right)
      _ <- compatibleKind(left, right)
      coordinates <- policy match
        case AxisConcatenationPolicy.AppendDeclaredCoordinates =>
          appendDeclaredCoordinates(left, right)
        case AxisConcatenationPolicy.RequireContinuous =>
          concatenateContinuous(left, right)
      result <- checked(left.name.value, left.kind, coordinates)
    yield result

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

  private def compatibleName(
      left: Axis,
      right: Axis
  ): Either[ImageError, Unit] =
    Either.cond(
      left.name == right.name,
      (),
      ImageError.AxisConcatenationNameMismatch(left.name, right.name)
    )

  private def compatibleKind(
      left: Axis,
      right: Axis
  ): Either[ImageError, Unit] =
    Either.cond(
      left.kind == right.kind,
      (),
      ImageError.AxisConcatenationKindMismatch(left.kind, right.kind)
    )

  private def selectedOrdinalRecord(
      values: Vector[Int]
  ): AxisCoordinatesRecord =
    if values == Vector.range(0, values.size) then AxisCoordinatesRecord.Ordinal(values.size)
    else AxisCoordinatesRecord.OrdinalValues(values)

  private def selectedRegularRecord(
      indices: Vector[Int],
      origin: Double,
      step: Double,
      unit: String
  ): AxisCoordinatesRecord =
    val selected = indices.map(index => origin + index.toDouble * step)
    if indices.size == 1 then AxisCoordinatesRecord.Regular(1, selected.head, step, unit)
    else
      val indexStep = indices(1) - indices.head
      val regularIndices =
        indexStep != 0 && indices.sliding(2).forall {
          case Vector(left, right) => right - left == indexStep
          case _ => true
        }
      val selectedStep = step * indexStep.toDouble
      val reconstructsExactly =
        regularIndices && selected.indices.forall(index =>
          selected.head + index.toDouble * selectedStep == selected(index)
        )
      if reconstructsExactly then
        AxisCoordinatesRecord.Regular(
          selected.size,
          selected.head,
          selectedStep,
          unit
        )
      else AxisCoordinatesRecord.Explicit(selected, unit)

  private def appendDeclaredCoordinates(
      left: Axis,
      right: Axis
  ): Either[ImageError, AxisCoordinatesRecord] =
    val leftRecord = left.record.coordinates
    val rightRecord = right.record.coordinates
    (ordinalValues(leftRecord), ordinalValues(rightRecord)) match
      case (Some(leftValues), Some(rightValues)) =>
        Right(
          AxisCoordinatesRecord.OrdinalValues(
            leftValues ++ rightValues
          )
        )
      case _ =>
        (numericValues(leftRecord), numericValues(rightRecord)) match
          case (Some((leftValues, leftUnit)), Some((rightValues, rightUnit))) =>
            compatibleUnit(left.name, leftUnit, rightUnit).map(_ =>
              AxisCoordinatesRecord.Explicit(
                leftValues ++ rightValues,
                leftUnit
              )
            )
          case _ =>
            (leftRecord, rightRecord) match
              case (
                    AxisCoordinatesRecord.Categorical(leftLabels),
                    AxisCoordinatesRecord.Categorical(rightLabels)
                  ) =>
                Right(
                  AxisCoordinatesRecord.Categorical(leftLabels ++ rightLabels)
                )
              case _ =>
                Left(
                  ImageError.AxisConcatenationCoordinateMismatch(
                    left.name,
                    leftRecord,
                    rightRecord
                  )
                )

  private def concatenateContinuous(
      left: Axis,
      right: Axis
  ): Either[ImageError, AxisCoordinatesRecord] =
    (left.record.coordinates, right.record.coordinates) match
      case (
            AxisCoordinatesRecord.Regular(leftExtent, leftOrigin, leftStep, leftUnit),
            AxisCoordinatesRecord.Regular(rightExtent, rightOrigin, rightStep, rightUnit)
          ) =>
        for
          unit <- compatibleUnit(left.name, leftUnit, rightUnit)
          _ <- Either.cond(
            leftStep == rightStep,
            (),
            ImageError.AxisConcatenationStepMismatch(
              left.name,
              leftStep,
              rightStep,
              unit
            )
          )
          expected = leftOrigin + leftExtent.toDouble * leftStep
          leftLast = leftOrigin + (leftExtent - 1).toDouble * leftStep
          _ <- continuousBoundary(
            left.name,
            leftLast,
            expected,
            rightOrigin,
            leftStep,
            unit
          )
        yield AxisCoordinatesRecord.Regular(
          leftExtent + rightExtent,
          leftOrigin,
          leftStep,
          leftUnit
        )
      case (leftRecord, rightRecord) =>
        (ordinalValues(leftRecord), ordinalValues(rightRecord)) match
          case (Some(leftValues), Some(rightValues)) =>
            val expected = leftValues.last + 1
            if !consecutiveOrdinals(leftValues) || !consecutiveOrdinals(rightValues)
            then
              Left(
                ImageError.AxisConcatenationContinuityUnavailable(
                  left.name,
                  leftRecord,
                  rightRecord
                )
              )
            else if rightValues.head == expected then
              Right(AxisCoordinatesRecord.OrdinalValues(leftValues ++ rightValues))
            else if rightValues.head <= leftValues.last then
              Left(
                ImageError.AxisConcatenationOverlap(
                  left.name,
                  AxisCoordinate.Ordinal(leftValues.last),
                  AxisCoordinate.Ordinal(rightValues.head)
                )
              )
            else
              Left(
                ImageError.AxisConcatenationDiscontinuity(
                  left.name,
                  AxisCoordinate.Ordinal(expected),
                  AxisCoordinate.Ordinal(rightValues.head)
                )
              )
          case _ =>
            Left(
              ImageError.AxisConcatenationContinuityUnavailable(
                left.name,
                leftRecord,
                rightRecord
              )
            )

  private def compatibleUnit(
      name: AxisName,
      left: String,
      right: String
  ): Either[ImageError, AxisUnit] =
    for
      leftUnit <- AxisUnit.fromId(left)
      rightUnit <- AxisUnit.fromId(right)
      _ <- Either.cond(
        leftUnit == rightUnit,
        (),
        ImageError.AxisConcatenationUnitMismatch(
          name,
          leftUnit,
          rightUnit
        )
      )
    yield leftUnit

  private def continuousBoundary(
      name: AxisName,
      leftLast: Double,
      expected: Double,
      actual: Double,
      step: Double,
      unit: AxisUnit
  ): Either[ImageError, Unit] =
    if actual == expected then Right(())
    else if (step > 0.0 && actual <= leftLast) ||
      (step < 0.0 && actual >= leftLast)
    then
      Left(
        ImageError.AxisConcatenationOverlap(
          name,
          AxisCoordinate.Numeric(leftLast, unit),
          AxisCoordinate.Numeric(actual, unit)
        )
      )
    else
      Left(
        ImageError.AxisConcatenationDiscontinuity(
          name,
          AxisCoordinate.Numeric(expected, unit),
          AxisCoordinate.Numeric(actual, unit)
        )
      )

  private def ordinalValues(record: AxisCoordinatesRecord): Option[Vector[Int]] =
    record match
      case AxisCoordinatesRecord.Ordinal(extent) =>
        Some(Vector.range(0, extent))
      case AxisCoordinatesRecord.OrdinalValues(values) =>
        Some(values)
      case _ =>
        None

  private def numericValues(
      record: AxisCoordinatesRecord
  ): Option[(Vector[Double], String)] =
    record match
      case AxisCoordinatesRecord.Regular(extent, origin, step, unit) =>
        Some(
          Vector.tabulate(extent)(index => origin + index.toDouble * step) -> unit
        )
      case AxisCoordinatesRecord.Explicit(values, unit) =>
        Some(values -> unit)
      case _ =>
        None

  private def consecutiveOrdinals(values: Vector[Int]): Boolean =
    values.sliding(2).forall {
      case Vector(left, right) => right == left + 1
      case _ => true
    }

/** Policy governing the coordinate boundary when concatenating two axes. */
enum AxisConcatenationPolicy derives CanEqual:
  /** Append every declared coordinate in order, permitting gaps and repeated coordinates. */
  case AppendDeclaredCoordinates

  /** Require both axes and their boundary to form one continuous regular sequence. */
  case RequireContinuous

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
    else if count > 1 then Left(ImageError.AmbiguousNonSpatialAxisKind(kind, count))
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
    else Right(new NonSpatialAxes(copied.map(values)))

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
      case None => Right(new NonSpatialAxes(copied))

  def fromRecords(
      records: IterableOnce[AxisRecord]
  ): Either[ImageError, NonSpatialAxes] =
    records.iterator
      .foldLeft[
        Either[ImageError, Vector[Axis]]
      ](Right(Vector.empty)) { (accumulated, record) =>
        for
          axes <- accumulated
          axis <- Axis.fromRecord(record)
        yield axes :+ axis
      }
      .flatMap(from)

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
