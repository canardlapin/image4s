package image4s.geometry

opaque type FrameId = String

object FrameId:
  def parse(value: String): Either[GeometryError, FrameId] =
    Identifier
      .validate(value)
      .toRight(GeometryError.InvalidFrameId(value))

  extension (id: FrameId) def value: String = id

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

enum LengthUnit derives CanEqual:
  case Millimeter, Meter, Micrometer

enum CoordinateConvention derives CanEqual:
  case Unspecified, RAS, LPS

/** Presentation-only metadata for a physical frame.
  *
  * Units and coordinate convention are structural and therefore live in [[FrameKey]], not in this
  * renameable description.
  */
final case class FrameMetadata private (
    label: String
) derives CanEqual

object FrameMetadata:
  def create(label: String): Either[GeometryError, FrameMetadata] =
    val normalized = label.trim
    if normalized.nonEmpty && normalized == label then Right(new FrameMetadata(label))
    else Left(GeometryError.InvalidFrameLabel(label))

  def named(label: String): Either[GeometryError, FrameMetadata] =
    create(label)

/** Persistent structural identity of a physical frame. */
final case class FrameKey(
    id: FrameId,
    spatialRank: Int,
    unit: LengthUnit,
    convention: CoordinateConvention
) derives CanEqual

/** Neutral frame persistence record.
  *
  * Metadata is presentation-only and does not participate in key conflicts.
  */
final case class FrameRecord(
    key: FrameKey,
    metadata: FrameMetadata
) derives CanEqual

final class Frame[D <: Dim] private (
    val metadata: FrameMetadata,
    val unit: LengthUnit,
    val convention: CoordinateConvention,
    val spatialRank: Int,
    val persistentKey: Option[FrameKey],
    private val runtimeToken: Frame.RuntimeToken
):
  def persistentId: Option[FrameId] =
    persistentKey.map(_.id)

  def record: Either[GeometryError, FrameRecord] =
    persistentKey
      .map(key => FrameRecord(key, metadata))
      .toRight(GeometryError.EphemeralFrameHasNoRecord)

  def sameRuntimeOwnerAs(other: Frame[?]): Boolean =
    runtimeToken.sameAs(other.runtimeToken)

  def samePersistentKeyAs(other: Frame[?]): Boolean =
    persistentKey.nonEmpty && persistentKey == other.persistentKey

  private[geometry] def mismatchWith(other: Frame[?]): GeometryError =
    (persistentKey, other.persistentKey) match
      case (Some(left), Some(right)) if left == right =>
        GeometryError.FrameOwnerMismatch(left.id)
      case (Some(left), Some(right)) =>
        GeometryError.FrameMismatch(left.id, right.id)
      case _ =>
        GeometryError.EphemeralFrameMismatch

  override def toString: String =
    val identity =
      persistentId.map(_.value).getOrElse("ephemeral")
    s"Frame($identity, ${metadata.label}, D$spatialRank, $unit, $convention)"

object Frame:
  private final class RuntimeToken:
    def sameAs(other: RuntimeToken): Boolean =
      this eq other

  private final case class RegistryEntry(
      key: FrameKey,
      value: Frame[?]
  )

  /** Immutable persistent-frame registry.
    *
    * Registering or restoring returns a new value. Existing registry instances remain safe to share
    * across threads and Scala.js tasks.
    */
  final class Registry private[Frame] (
      private[Frame] val entries: Map[FrameId, RegistryEntry]
  ):
    def size: Int =
      entries.size

    def register[D <: Dim](
        frame: Frame[D]
    ): Either[GeometryError, Registry] =
      Frame.register(frame, this)

  object Registry:
    val empty: Registry =
      new Registry(Map.empty)

  final class Resolution[D <: Dim] private[Frame] (
      val frame: Frame[D],
      val registry: Registry
  )

  def ephemeral[D <: Dim](
      metadata: FrameMetadata,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using dimension: Dimension[D]): Frame[D] =
    new Frame(
      metadata,
      unit,
      convention,
      dimension.rank,
      None,
      new RuntimeToken()
    )

  def named[D <: Dim](
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using dimension: Dimension[D]): Either[GeometryError, Frame[D]] =
    FrameMetadata
      .named(label)
      .map(metadata => ephemeral(metadata, unit, convention))

  def createPersistent[D <: Dim](
      id: FrameId,
      metadata: FrameMetadata,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using dimension: Dimension[D]): Frame[D] =
    val key = FrameKey(id, dimension.rank, unit, convention)
    new Frame(
      metadata,
      unit,
      convention,
      dimension.rank,
      Some(key),
      new RuntimeToken()
    )

  def persistentNamed[D <: Dim](
      id: FrameId,
      label: String,
      unit: LengthUnit = LengthUnit.Millimeter,
      convention: CoordinateConvention = CoordinateConvention.Unspecified
  )(using dimension: Dimension[D]): Either[GeometryError, Frame[D]] =
    FrameMetadata
      .named(label)
      .map(metadata => createPersistent(id, metadata, unit, convention))

  def restore[D <: Dim](
      record: FrameRecord,
      registry: Registry
  )(using dimension: Dimension[D]): Either[GeometryError, Resolution[D]] =
    val key = record.key
    if key.spatialRank != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, key.spatialRank))
    else
      registry.entries.get(key.id) match
        case None =>
          val restored =
            new Frame[D](
              record.metadata,
              key.unit,
              key.convention,
              key.spatialRank,
              Some(key),
              new RuntimeToken()
            )
          val updated =
            new Registry(
              registry.entries.updated(
                key.id,
                RegistryEntry(key, restored)
              )
            )
          Right(new Resolution(restored, updated))
        case Some(entry) if entry.key == key =>
          // Retype the immutable header after the checked rank/key match while
          // retaining the registry's live-owner token.
          Right(
            new Resolution(
              new Frame[D](
                entry.value.metadata,
                key.unit,
                key.convention,
                key.spatialRank,
                Some(key),
                entry.value.runtimeToken
              ),
              registry
            )
          )
        case Some(entry) =>
          Left(
            GeometryError.FrameKeyConflict(
              key.id,
              entry.key,
              key
            )
          )

  def restoreDynamic(
      record: FrameRecord,
      registry: Registry
  ): Either[GeometryError, (SomeFrame, Registry)] =
    record.key.spatialRank match
      case 2 =>
        restore[D2](record, registry).map(resolution =>
          SomeFrame.pack[D2](resolution.frame) -> resolution.registry
        )
      case 3 =>
        restore[D3](record, registry).map(resolution =>
          SomeFrame.pack[D3](resolution.frame) -> resolution.registry
        )
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

  /** Check alignment after exact singleton owner types have been erased.
    *
    * Ordinary code should use [[align]], which retains path-dependent owners. Generic boundaries
    * can use this evidence instead of invoking a widened point or vector constructor.
    */
  def alignOwners[
      D <: Dim,
      A <: Frame[D],
      B <: Frame[D]
  ](
      left: A,
      right: B
  ): Either[GeometryError, FrameAlignment[D, A, B]] =
    FrameAlignment.checkOwners(left, right)

  private def register[D <: Dim](
      frame: Frame[D],
      registry: Registry
  ): Either[GeometryError, Registry] =
    frame.persistentKey match
      case None =>
        Left(GeometryError.CannotRegisterEphemeralFrame)
      case Some(key) =>
        registry.entries.get(key.id) match
          case None =>
            Right(
              new Registry(
                registry.entries.updated(
                  key.id,
                  RegistryEntry(key, frame)
                )
              )
            )
          case Some(entry) if entry.key == key && (entry.value eq frame) =>
            Right(registry)
          case Some(entry) if entry.key != key =>
            Left(
              GeometryError.FrameKeyConflict(
                key.id,
                entry.key,
                key
              )
            )
          case Some(_) =>
            Left(GeometryError.FrameRestoreDuplicateOwner(key.id))

type FrameRegistry = Frame.Registry

object FrameRegistry:
  val empty: FrameRegistry =
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
    if point.belongsTo(left) then Point.fromVectorAs(right, point.coordinates)
    else Left(left.mismatchWith(point.frame))

  def pointToLeft(
      point: Point[B, D]
  )(using Dimension[D]): Either[GeometryError, Point[A, D]] =
    if point.belongsTo(right) then Point.fromVectorAs(left, point.coordinates)
    else Left(right.mismatchWith(point.frame))

  def vectorToRight(
      vector: Vec[A, D]
  )(using Dimension[D]): Either[GeometryError, Vec[B, D]] =
    if vector.belongsTo(left) then Vec.fromVectorAs(right, vector.coordinates)
    else Left(left.mismatchWith(vector.frame))

  def vectorToLeft(
      vector: Vec[B, D]
  )(using Dimension[D]): Either[GeometryError, Vec[A, D]] =
    if vector.belongsTo(right) then Vec.fromVectorAs(left, vector.coordinates)
    else Left(right.mismatchWith(vector.frame))

object FrameAlignment:
  def check[D <: Dim](
      left: Frame[D],
      right: Frame[D]
  ): Either[
    GeometryError,
    FrameAlignment[D, left.type, right.type]
  ] =
    if left.sameRuntimeOwnerAs(right) || left.samePersistentKeyAs(right) then
      Right(new FrameAlignment(left, right))
    else Left(left.mismatchWith(right))

  private[geometry] def checkOwners[
      D <: Dim,
      A <: Frame[D],
      B <: Frame[D]
  ](
      left: A,
      right: B
  ): Either[GeometryError, FrameAlignment[D, A, B]] =
    if left.sameRuntimeOwnerAs(right) || left.samePersistentKeyAs(right) then
      Right(new FrameAlignment(left, right))
    else Left(left.mismatchWith(right))
