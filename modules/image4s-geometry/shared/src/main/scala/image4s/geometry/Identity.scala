package image4s.geometry

/** Shared coordinate identities are aliases, never a second live frame owner. */
type FrameId = spatial4s.FrameId
object FrameId:
  def parse(value: String): Either[GeometryError, FrameId] =
    spatial4s.FrameId.parse(value).left.map(GeometryError.fromSpatial)
  extension (id: FrameId) def value: String = spatial4s.FrameId.value(id)

opaque type GridId = String

object GridId:
  def parse(value: String): Either[GeometryError, GridId] =
    Identifier
      .validate(value)
      .toRight(GeometryError.InvalidGridId(value))

  extension (id: GridId) def value: String = id

private object Identifier:
  def validate(value: String): Option[String] =
    val normalized = value.trim
    Option.when(normalized.nonEmpty && normalized == value)(value)

type LengthUnit = spatial4s.CoordinateUnit
object LengthUnit:
  val Millimeter: LengthUnit = spatial4s.CoordinateUnit.Millimeter
  val Meter: LengthUnit = spatial4s.CoordinateUnit.Meter
  val Micrometer: LengthUnit = spatial4s.CoordinateUnit.Micrometer
  def values: Array[LengthUnit] = Array(Millimeter, Meter, Micrometer)
  def serializedName(unit: LengthUnit): String =
    if unit == Millimeter then "Millimeter"
    else if unit == Meter then "Meter"
    else if unit == Micrometer then "Micrometer"
    else
      val quantity = unit.quantity match
        case spatial4s.CoordinateQuantity.Length => "length"
        case spatial4s.CoordinateQuantity.Dimensionless => "dimensionless"
        case other => s"custom:${other.id}"
      val scale = unit.scaleToCanonical.fold("none")(value =>
        java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))
      )
      Vector(unit.id, unit.symbol, quantity, scale)
        .map(value => s"${value.length}:$value")
        .mkString("custom-unit:", "|", "")

type CoordinateConvention = spatial4s.CoordinateConvention
object CoordinateConvention:
  val Unspecified: CoordinateConvention = spatial4s.CoordinateConvention.Unspecified
  val RAS: CoordinateConvention = spatial4s.CoordinateConvention.RAS
  val LPS: CoordinateConvention = spatial4s.CoordinateConvention.LPS
  def values: Array[CoordinateConvention] = Array(Unspecified, RAS, LPS)
  def serializedName(convention: CoordinateConvention): String =
    if convention == Unspecified then "Unspecified" else convention.id

type FrameMetadata = spatial4s.FrameMetadata
object FrameMetadata:
  def named(label: String): Either[GeometryError, FrameMetadata] =
    spatial4s.FrameMetadata.named(label).left.map(GeometryError.fromSpatial)
  def create(label: String): Either[GeometryError, FrameMetadata] = named(label)

type FrameKey = spatial4s.FrameKey
val FrameKey = spatial4s.FrameKey
type FrameRecord = spatial4s.FrameRecord
val FrameRecord = spatial4s.FrameRecord
type Frame[D <: Dim] = spatial4s.Frame[D]
type FrameRegistry = spatial4s.FrameRegistry
object FrameRegistry:
  val empty: FrameRegistry = spatial4s.FrameRegistry.empty

type FrameAlignment[D <: Dim, A <: Frame[D], B <: Frame[D]] = spatial4s.FrameAlignment[D, A, B]
object FrameAlignment:
  def check[D <: Dim](
      left: Frame[D],
      right: Frame[D]
  ): Either[GeometryError, FrameAlignment[D, left.type, right.type]] =
    Frame.align(left, right)

type SomeFrame = spatial4s.SomeFrame
object Frame:
  type Registry = spatial4s.FrameRegistry
  object Registry:
    val empty: Registry = spatial4s.FrameRegistry.empty
  type Resolution[D <: Dim] = spatial4s.FrameResolution[D]

  def ephemeral[D <: Dim](
      metadata: FrameMetadata,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using Dimension[D]): Frame[D] =
    spatial4s.Frame.ephemeral(metadata, unit, convention)
  def named[D <: Dim](
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using Dimension[D]): Either[GeometryError, Frame[D]] =
    spatial4s.Frame.named(label, unit, convention).left.map(GeometryError.fromSpatial)
  def persistent[D <: Dim](
      id: FrameId,
      metadata: FrameMetadata,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using Dimension[D]): Frame[D] =
    spatial4s.Frame.persistent(id, metadata, unit, convention)
  def persistentNamed[D <: Dim](
      id: FrameId,
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using Dimension[D]): Either[GeometryError, Frame[D]] =
    spatial4s.Frame.persistentNamed(id, label, unit, convention).left.map(GeometryError.fromSpatial)
  def register[D <: Dim](frame: Frame[D], registry: Registry): Either[GeometryError, Registry] =
    registry.register(frame).left.map(GeometryError.fromSpatial)
  def restore[D <: Dim](record: FrameRecord, registry: Registry)(using
      Dimension[D]
  ): Either[GeometryError, Resolution[D]] =
    spatial4s.Frame.restore(record, registry).left.map(GeometryError.fromSpatial)
  def restoreDynamic(
      record: FrameRecord,
      registry: Registry
  ): Either[GeometryError, (SomeFrame, Registry)] =
    spatial4s.Frame.restoreDynamic(record, registry).left.map(GeometryError.fromSpatial)
  def align[D <: Dim](
      left: Frame[D],
      right: Frame[D]
  ): Either[GeometryError, FrameAlignment[D, left.type, right.type]] =
    spatial4s.Frame.align(left, right).left.map(_ => frameMismatch(left, right))
  def alignOwners[D <: Dim, A <: Frame[D], B <: Frame[D]](
      left: A,
      right: B
  ): Either[GeometryError, FrameAlignment[D, A, B]] =
    spatial4s.Frame.alignOwners(left, right).left.map(_ => frameMismatch(left, right))

private[geometry] def frameMismatch(expected: Frame[?], actual: Frame[?]): GeometryError =
  (expected.persistentKey, actual.persistentKey) match
    case (Some(left), Some(right)) if left == right => GeometryError.FrameOwnerMismatch(left.id)
    case (Some(left), Some(right)) => GeometryError.FrameMismatch(left.id, right.id)
    case _ => GeometryError.EphemeralFrameMismatch

extension (frame: Frame[?])
  private[geometry] def mismatchWith(other: Frame[?]): GeometryError = frameMismatch(frame, other)
