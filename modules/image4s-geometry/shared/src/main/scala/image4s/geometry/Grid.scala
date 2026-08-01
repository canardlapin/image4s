package image4s.geometry

/** Canonical affine values used in persistent grid identity.
  *
  * Affine construction tolerances and diagnostics are deliberately excluded.
  */
final case class CanonicalAffineRecord(
    rowMajor: Vector[Double]
) derives CanEqual

/** Persistent structural identity of a finite affine grid. */
final case class GridKey(
    id: GridId,
    frame: FrameKey,
    shape: Vector[Int],
    indexToFrame: CanonicalAffineRecord
) derives CanEqual:
  def spatialRank: Int =
    frame.spatialRank

/** Neutral persistent grid record. */
final case class GridRecord(
    key: GridKey
) derives CanEqual

/** Checked bounded index owned by one live grid. */
final class GridIndex[
    G <: Grid[?, D],
    D <: Dim
] private[geometry] (
    val grid: G,
    val lattice: LatticeIndex[D]
):
  def values: Vector[Int] =
    lattice.values

  def apply(axis: Int): Option[Int] =
    lattice(axis)

object GridIndex:
  private[geometry] def checked[
      F <: Frame[D],
      D <: Dim
  ](
      grid: Grid[F, D],
      lattice: LatticeIndex[D]
  ): Either[
    GeometryError,
    GridIndex[grid.type, D]
  ] =
    grid.shape.zip(lattice.values).zipWithIndex.collectFirst {
      case ((extent, value), axis) if value < 0 || value >= extent =>
        GeometryError.GridIndexOutOfBounds(axis, value, extent)
    } match
      case Some(error) => Left(error)
      case None        => Right(new GridIndex(grid, lattice))

final class Grid[F <: Frame[D], D <: Dim] private (
    val frame: F,
    val shape: Vector[Int],
    val indexToFrame: Affine[D],
    val dimension: Dimension[D],
    val persistentKey: Option[GridKey],
    private val runtimeToken: Grid.RuntimeToken
):
  def spatialRank: Int =
    shape.length

  def persistentId: Option[GridId] =
    persistentKey.map(_.id)

  def record: Either[GeometryError, GridRecord] =
    persistentKey
      .map(GridRecord.apply)
      .toRight(GeometryError.EphemeralGridHasNoRecord)

  def contains(index: LatticeIndex[D]): Boolean =
    index.values.indices.forall(axis =>
      index.values(axis) >= 0 && index.values(axis) < shape(axis)
    )

  def index(
      lattice: LatticeIndex[D]
  ): Either[GeometryError, GridIndex[this.type, D]] =
    GridIndex.checked(this, lattice)

  def index(
      coordinates: Int*
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, GridIndex[this.type, D]] =
    LatticeIndex
      .fromVector(coordinates.toVector)
      .flatMap(index)

  /** Map an unbounded lattice coordinate into the physical frame. */
  def pointAt(index: LatticeIndex[D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[F, D]] =
    Point.fromVectorAs[D, F](
      frame,
      indexToFrame.applyUnchecked(index.values.map(_.toDouble))
    )

  /** Map a checked index owned by this exact live grid. */
  def pointAt(index: GridIndex[this.type, D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[F, D]] =
    pointAt(index.lattice)

  def pointAt(continuousIndex: ContinuousIndex[D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[F, D]] =
    Point.fromVectorAs[D, F](
      frame,
      indexToFrame.applyUnchecked(continuousIndex.values)
    )

  def continuousIndexOf(point: Point[F, D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, ContinuousIndex[D]] =
    if !point.belongsTo(frame) then Left(frame.mismatchWith(point.frame))
    else
      ContinuousIndex.fromVector(
        indexToFrame.inverse.applyUnchecked(point.coordinates)
      )

  def sameRuntimeOwnerAs(other: Grid[?, ?]): Boolean =
    runtimeToken.sameAs(other.runtimeToken)

  def samePersistentKeyAs(other: Grid[?, ?]): Boolean =
    persistentKey.nonEmpty && persistentKey == other.persistentKey

  private[geometry] def mismatchWith(other: Grid[?, ?]): GeometryError =
    (persistentKey, other.persistentKey) match
      case (Some(left), Some(right)) if left == right =>
        GeometryError.GridOwnerMismatch(left.id)
      case (Some(left), Some(right)) =>
        GeometryError.GridMismatch(left.id, right.id)
      case _ =>
        GeometryError.EphemeralGridMismatch

object Grid:
  private final class RuntimeToken:
    def sameAs(other: RuntimeToken): Boolean =
      this eq other

  private final case class RegistryEntry(
      key: GridKey,
      frame: Frame[?],
      value: Grid[?, ?]
  )

  /** Immutable persistent-grid registry. */
  final class Registry private[Grid] (
      private[Grid] val entries: Map[GridId, RegistryEntry]
  ):
    def size: Int =
      entries.size

    def register[D <: Dim, F <: Frame[D]](
        grid: Grid[F, D]
    ): Either[GeometryError, Registry] =
      Grid.register(grid, this)

  object Registry:
    val empty: Registry =
      new Registry(Map.empty)

  final class Resolution[F <: Frame[D], D <: Dim] private[Grid] (
      val grid: Grid[F, D],
      val registry: Registry
  )

  def in[D <: Dim](
      frame: Frame[D]
  )(
      shape: IterableOnce[Int],
      indexToFrame: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[frame.type, D]] =
    create(
      frame,
      shape.iterator.toVector,
      indexToFrame,
      None
    )

  /** Construct an ephemeral grid while preserving a precise frame type. */
  def forFrame[D <: Dim, F <: Frame[D]](
      frame: F
  )(
      shape: IterableOnce[Int],
      indexToFrame: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[F, D]] =
    create(
      frame,
      shape.iterator.toVector,
      indexToFrame,
      None
    )

  def createPersistent[D <: Dim, F <: Frame[D]](
      id: GridId,
      frame: F
  )(
      shape: IterableOnce[Int],
      indexToFrame: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[F, D]] =
    frame.persistentKey match
      case None =>
        Left(GeometryError.PersistentGridRequiresPersistentFrame(id))
      case Some(frameKey) =>
        val copiedShape = shape.iterator.toVector
        val key =
          GridKey(
            id,
            frameKey,
            copiedShape,
            CanonicalAffineRecord(indexToFrame.rowMajor)
          )
        create(frame, copiedShape, indexToFrame, Some(key))

  def restore[D <: Dim, F <: Frame[D]](
      record: GridRecord,
      frame: F,
      registry: Registry,
      validationTolerance: Double = Affine.DefaultTolerance
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, Resolution[F, D]] =
    val key = record.key
    if key.spatialRank != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, key.spatialRank))
    else if !frame.persistentKey.contains(key.frame) then
      Left(
        GeometryError.GridFrameKeyMismatch(
          key.id,
          key.frame,
          frame.persistentKey
        )
      )
    else
      for
        affine <- Affine.fromRowMajor[D](
          key.indexToFrame.rowMajor,
          validationTolerance
        )
        _ <-
          Either.cond(
            affine.rowMajor == key.indexToFrame.rowMajor,
            (),
            GeometryError.NonCanonicalGridAffineRecord(key.id)
          )
        candidate <- create[D, F](
          frame,
          key.shape,
          affine,
          Some(key)
        )
        restored <- restoreRegistered(key, candidate, registry)
      yield restored

  def align[D <: Dim, LF <: Frame[D], RF <: Frame[D]](
      left: Grid[LF, D],
      right: Grid[RF, D]
  ): Either[GeometryError, GridAlignment[D, LF, RF]] =
    GridAlignment.check(left, right)

  def exactCongruence[D <: Dim, LF <: Frame[D], RF <: Frame[D]](
      left: Grid[LF, D],
      right: Grid[RF, D]
  ): Either[GeometryError, GridCongruence[D, LF, RF]] =
    for
      _ <- Frame.align(left.frame, right.frame)
      _ <-
        Either.cond(
          left.shape == right.shape &&
            left.indexToFrame.rowMajor == right.indexToFrame.rowMajor,
          (),
          GeometryError.GridsNotCongruent(0.0)
        )
    yield new GridCongruence(left, right, 0.0, true)

  def approximateCongruence[
      D <: Dim,
      LF <: Frame[D],
      RF <: Frame[D]
  ](
      left: Grid[LF, D],
      right: Grid[RF, D],
      tolerance: Double
  ): Either[GeometryError, GridCongruence[D, LF, RF]] =
    if !tolerance.isFinite || tolerance < 0.0 then
      Left(GeometryError.InvalidCongruenceTolerance(tolerance))
    else
      for
        _ <- Frame.align(left.frame, right.frame)
        sameShape = left.shape == right.shape
        sameAffine =
          left.indexToFrame.rowMajor
            .zip(right.indexToFrame.rowMajor)
            .forall { case (leftValue, rightValue) =>
              math.abs(leftValue - rightValue) <= tolerance
            }
        _ <-
          Either.cond(
            sameShape && sameAffine,
            (),
            GeometryError.GridsNotCongruent(tolerance)
          )
      yield new GridCongruence(
        left,
        right,
        tolerance,
        left.indexToFrame.rowMajor == right.indexToFrame.rowMajor
      )

  private def create[D <: Dim, F <: Frame[D]](
      frame: F,
      shape: Vector[Int],
      indexToFrame: Affine[D],
      key: Option[GridKey]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[F, D]] =
    if shape.length != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, shape.length))
    else
      shape.zipWithIndex.collectFirst {
        case (extent, axis) if extent <= 0 =>
          GeometryError.NonPositiveGridExtent(axis, extent)
      } match
        case Some(error) =>
          Left(error)
        case None =>
          Right(
            new Grid(
              frame,
              shape,
              indexToFrame,
              dimension,
              key,
              new RuntimeToken()
            )
          )

  private def register[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      registry: Registry
  ): Either[GeometryError, Registry] =
    grid.persistentKey match
      case None =>
        Left(GeometryError.CannotRegisterEphemeralGrid)
      case Some(key) =>
        registry.entries.get(key.id) match
          case None =>
            Right(
              new Registry(
                registry.entries.updated(
                  key.id,
                  RegistryEntry(key, grid.frame, grid)
                )
              )
            )
          case Some(entry)
              if entry.key == key &&
                entry.frame.sameRuntimeOwnerAs(grid.frame) &&
                (entry.value eq grid) =>
            Right(registry)
          case Some(entry) if entry.key != key =>
            Left(
              GeometryError.GridKeyConflict(
                key.id,
                entry.key,
                key
              )
            )
          case Some(entry)
              if !entry.frame.sameRuntimeOwnerAs(grid.frame) =>
            Left(
              GeometryError.GridRestoreFrameOwnerConflict(
                key.id,
                key.frame.id
              )
            )
          case Some(_) =>
            Left(GeometryError.GridRestoreDuplicateOwner(key.id))

  private def restoreRegistered[D <: Dim, F <: Frame[D]](
      key: GridKey,
      candidate: Grid[F, D],
      registry: Registry
  ): Either[GeometryError, Resolution[F, D]] =
    registry.entries.get(key.id) match
      case None =>
        val updated =
          new Registry(
            registry.entries.updated(
              key.id,
              RegistryEntry(key, candidate.frame, candidate)
            )
          )
        Right(new Resolution(candidate, updated))
      case Some(entry) if entry.key != key =>
        Left(
          GeometryError.GridKeyConflict(
            key.id,
            entry.key,
            key
          )
        )
      case Some(entry)
          if !entry.frame.sameRuntimeOwnerAs(candidate.frame) =>
        Left(
          GeometryError.GridRestoreFrameOwnerConflict(
            key.id,
            key.frame.id
          )
        )
      case Some(entry) =>
        // Retype the immutable header after the checked key/frame-owner match
        // while retaining the registry's live grid-owner token.
        Right(
          new Resolution(
            new Grid(
              candidate.frame,
              candidate.shape,
              candidate.indexToFrame,
              candidate.dimension,
              candidate.persistentKey,
              entry.value.runtimeToken
            ),
            registry
          )
        )

type GridRegistry = Grid.Registry

object GridRegistry:
  val empty: GridRegistry =
    Grid.Registry.empty

final class GridAlignment[
    D <: Dim,
    LF <: Frame[D],
    RF <: Frame[D]
] private (
    val left: Grid[LF, D],
    val right: Grid[RF, D]
):
  def sameRuntimeOwner: Boolean =
    left.sameRuntimeOwnerAs(right)

  def pointToRight(
      point: Point[LF, D]
  )(using Dimension[D]): Either[GeometryError, Point[RF, D]] =
    if point.belongsTo(left.frame) then
      Point.fromVectorAs(right.frame, point.coordinates)
    else Left(left.frame.mismatchWith(point.frame))

  def pointToLeft(
      point: Point[RF, D]
  )(using Dimension[D]): Either[GeometryError, Point[LF, D]] =
    if point.belongsTo(right.frame) then
      Point.fromVectorAs(left.frame, point.coordinates)
    else Left(right.frame.mismatchWith(point.frame))

object GridAlignment:
  def check[D <: Dim, LF <: Frame[D], RF <: Frame[D]](
      left: Grid[LF, D],
      right: Grid[RF, D]
  ): Either[GeometryError, GridAlignment[D, LF, RF]] =
    if left.sameRuntimeOwnerAs(right) || left.samePersistentKeyAs(right) then
      Frame.align(left.frame, right.frame).map(_ =>
        new GridAlignment(left, right)
      )
    else Left(left.mismatchWith(right))

final class GridCongruence[
    D <: Dim,
    LF <: Frame[D],
    RF <: Frame[D]
] private[geometry] (
    val left: Grid[LF, D],
    val right: Grid[RF, D],
    val tolerance: Double,
    val exact: Boolean
)
