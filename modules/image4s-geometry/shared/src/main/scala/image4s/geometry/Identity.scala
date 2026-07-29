package image4s.geometry

import scala.collection.mutable
import scala.util.Random

opaque type FrameId = String

object FrameId:
  def parse(value: String): Either[GeometryError, FrameId] =
    Identifier
      .validate(value)
      .toRight(GeometryError.InvalidFrameId(value))

  private[geometry] def fresh(): FrameId =
    Identifier.fresh("frame")

  extension (id: FrameId)
    def value: String = id

opaque type GridId = String

object GridId:
  def parse(value: String): Either[GeometryError, GridId] =
    Identifier
      .validate(value)
      .toRight(GeometryError.InvalidGridId(value))

  private[geometry] def fresh(): GridId =
    Identifier.fresh("grid")

  extension (id: GridId)
    def value: String = id

private object Identifier:
  def validate(value: String): Option[String] =
    val normalized = value.trim
    Option.when(normalized.nonEmpty && normalized == value)(value)

  def fresh(prefix: String): String =
    val high = java.lang.Long.toHexString(Random.nextLong())
    val low = java.lang.Long.toHexString(Random.nextLong())
    s"$prefix-$high-$low"

enum LengthUnit derives CanEqual:
  case Millimeter, Meter, Micrometer

enum CoordinateConvention derives CanEqual:
  case Unspecified, RAS, LPS

final case class FrameMetadata private (
    label: String,
    unit: LengthUnit,
    convention: CoordinateConvention
) derives CanEqual

object FrameMetadata:
  def create(
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  ): Either[GeometryError, FrameMetadata] =
    val normalized = label.trim
    if normalized.nonEmpty && normalized == label then
      Right(new FrameMetadata(label, unit, convention))
    else Left(GeometryError.InvalidFrameLabel(label))

  def named(label: String): Either[GeometryError, FrameMetadata] =
    create(label)

final case class FrameRecord(
    id: FrameId,
    spatialRank: Int,
    metadata: FrameMetadata
) derives CanEqual

final class Frame[D <: Dim] private (
    val id: FrameId,
    val metadata: FrameMetadata,
    val spatialRank: Int,
    private val runtimeToken: Frame.RuntimeToken
):
  def record: FrameRecord =
    FrameRecord(id, spatialRank, metadata)

  private[geometry] def sameRuntimeOwnerAs(other: Frame[?]): Boolean =
    runtimeToken.sameAs(other.runtimeToken)

  private[geometry] def mismatchWith(other: Frame[?]): GeometryError =
    if id == other.id then GeometryError.FrameOwnerMismatch(id)
    else GeometryError.FrameMismatch(id, other.id)

  override def toString: String =
    s"Frame(${id.value}, ${metadata.label}, D$spatialRank)"

object Frame:
  private final class RuntimeToken:
    def sameAs(other: RuntimeToken): Boolean =
      this eq other

  final class Registry private[Frame] (
      private[Frame] val entries: mutable.Map[FrameId, (FrameRecord, AnyRef)]
  ):
    def register[D <: Dim](
        frame: Frame[D]
    ): Either[GeometryError, Unit] =
      Frame.register(frame, this)

  object Registry:
    def empty: Registry =
      new Registry(mutable.HashMap.empty)

  def fresh[D <: Dim](
      metadata: FrameMetadata
  )(using dimension: Dimension[D]): Frame[D] =
    new Frame(
      FrameId.fresh(),
      metadata,
      dimension.rank,
      new RuntimeToken()
    )

  def named[D <: Dim](
      label: String
  )(using dimension: Dimension[D]): Either[GeometryError, Frame[D]] =
    FrameMetadata.named(label).map(fresh[D])

  def restore[D <: Dim](
      record: FrameRecord,
      registry: Registry
  )(using dimension: Dimension[D]): Either[GeometryError, Frame[D]] =
    if record.spatialRank != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, record.spatialRank))
    else
      registry.entries.get(record.id) match
        case None =>
          val restored =
            new Frame[D](
              record.id,
              record.metadata,
              record.spatialRank,
              new RuntimeToken()
            )
          registry.entries.update(record.id, record -> restored)
          Right(restored)
        case Some((stored, value)) if stored == record =>
          // D is fixed by the sealed Dimension witness and the checked record
          // rank. The registry entry was created only by this companion.
          Right(value.asInstanceOf[Frame[D]])
        case Some(_) =>
          Left(GeometryError.FrameRestoreMetadataConflict(record.id))

  def restoreDynamic(
      record: FrameRecord,
      registry: Registry
  ): Either[GeometryError, SomeFrame] =
    record.spatialRank match
      case 2 =>
        restore[D2](record, registry).map(SomeFrame.pack[D2])
      case 3 =>
        restore[D3](record, registry).map(SomeFrame.pack[D3])
      case rank =>
        Left(GeometryError.UnsupportedSpatialRank(rank))

  def align[D <: Dim](
      left: Frame[D],
      right: Frame[D]
  ): Either[
    GeometryError,
    FrameAlignment[D, left.type, right.type]
  ] =
    FrameAlignment.check(left, right)

  private def register[D <: Dim](
      frame: Frame[D],
      registry: Registry
  ): Either[GeometryError, Unit] =
    registry.entries.get(frame.id) match
      case None =>
        registry.entries.update(frame.id, frame.record -> frame)
        Right(())
      case Some((record, value)) if record == frame.record && (value eq frame) =>
        Right(())
      case Some((record, _)) if record != frame.record =>
        Left(GeometryError.FrameRestoreMetadataConflict(frame.id))
      case Some(_) =>
        Left(GeometryError.FrameRestoreDuplicateOwner(frame.id))

type FrameRegistry = Frame.Registry

object FrameRegistry:
  def empty: FrameRegistry =
    Frame.Registry.empty

sealed trait SomeFrame:
  type D <: Dim
  val dimension: Dimension[D]
  val value: Frame[D]

object SomeFrame:
  private[geometry] def pack[A <: Dim](
      frame: Frame[A]
  )(using witness: Dimension[A]): SomeFrame { type D = A } =
    new SomeFrame:
      type D = A
      val dimension: Dimension[A] = witness
      val value: Frame[A] = frame

final class FrameAlignment[
    D <: Dim,
    A <: Frame[D],
    B <: Frame[D]
] private (
    val left: A,
    val right: B
):
  def sameRuntimeOwner: Boolean =
    left.sameRuntimeOwnerAs(right)

  def pointToRight(
      point: Point[A, D]
  )(using Dimension[D]): Either[GeometryError, Point[B, D]] =
    if point.belongsTo(left) then Point.fromVector(right, point.coordinates)
    else Left(left.mismatchWith(point.frame))

  def pointToLeft(
      point: Point[B, D]
  )(using Dimension[D]): Either[GeometryError, Point[A, D]] =
    if point.belongsTo(right) then Point.fromVector(left, point.coordinates)
    else Left(right.mismatchWith(point.frame))

  def vectorToRight(
      vector: Vec[A, D]
  )(using Dimension[D]): Either[GeometryError, Vec[B, D]] =
    if vector.belongsTo(left) then Vec.fromVector(right, vector.coordinates)
    else Left(left.mismatchWith(vector.frame))

  def vectorToLeft(
      vector: Vec[B, D]
  )(using Dimension[D]): Either[GeometryError, Vec[A, D]] =
    if vector.belongsTo(right) then Vec.fromVector(left, vector.coordinates)
    else Left(right.mismatchWith(vector.frame))

object FrameAlignment:
  def check[D <: Dim](
      left: Frame[D],
      right: Frame[D]
  ): Either[
    GeometryError,
    FrameAlignment[D, left.type, right.type]
  ] =
    if left.id != right.id then
      Left(GeometryError.FrameMismatch(left.id, right.id))
    else if left.record != right.record then
      Left(GeometryError.FrameRestoreMetadataConflict(left.id))
    else Right(new FrameAlignment(left, right))
