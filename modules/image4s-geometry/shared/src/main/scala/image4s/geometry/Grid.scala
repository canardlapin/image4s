package image4s.geometry

import scala.collection.mutable

final case class GridRecord(
    id: GridId,
    frameId: FrameId,
    spatialRank: Int,
    shape: Vector[Int],
    indexToFrameRowMajor: Vector[Double],
    affineTolerance: Double
) derives CanEqual

final class Grid[F <: Frame[D], D <: Dim] private (
    val id: GridId,
    val frame: F,
    val shape: Vector[Int],
    val indexToFrame: Affine[D],
    val dimension: Dimension[D],
    private val runtimeToken: Grid.RuntimeToken
):
  def spatialRank: Int =
    shape.length

  def record: GridRecord =
    GridRecord(
      id,
      frame.id,
      spatialRank,
      shape,
      indexToFrame.rowMajor,
      indexToFrame.tolerance
    )

  def contains(index: Index[D]): Boolean =
    index.values.indices.forall(axis =>
      index.values(axis) >= 0 && index.values(axis) < shape(axis)
    )

  def pointAt(index: Index[D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[F, D]] =
    Point.fromVector[D, F](
      frame,
      indexToFrame.applyUnchecked(index.values.map(_.toDouble))
    )

  def pointAt(continuousIndex: ContinuousIndex[D])(using
      dimension: Dimension[D]
  ): Either[GeometryError, Point[F, D]] =
    Point.fromVector[D, F](
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

  private[geometry] def sameRuntimeOwnerAs(other: Grid[?, ?]): Boolean =
    runtimeToken.sameAs(other.runtimeToken)

  private[geometry] def mismatchWith(other: Grid[?, ?]): GeometryError =
    if id == other.id then GeometryError.GridOwnerMismatch(id)
    else GeometryError.GridMismatch(id, other.id)

object Grid:
  private final class RuntimeToken:
    def sameAs(other: RuntimeToken): Boolean =
      this eq other

  private final case class RegistryEntry(
      record: GridRecord,
      frame: Frame[?],
      value: AnyRef
  )

  final class Registry private[Grid] (
      private[Grid] val entries: mutable.Map[GridId, RegistryEntry]
  ):
    def register[D <: Dim, F <: Frame[D]](
        grid: Grid[F, D]
    ): Either[GeometryError, Unit] =
      Grid.register(grid, this)

  object Registry:
    def empty: Registry =
      new Registry(mutable.HashMap.empty)

  def in[D <: Dim](
      frame: Frame[D]
  )(
      shape: IterableOnce[Int],
      indexToFrame: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[frame.type, D]] =
    create(
      GridId.fresh(),
      frame,
      shape.iterator.toVector,
      indexToFrame
    )

  /** Construct a grid while preserving an already precise frame type. */
  def forFrame[D <: Dim, F <: Frame[D]](
      frame: F
  )(
      shape: IterableOnce[Int],
      indexToFrame: Affine[D]
  )(using dimension: Dimension[D]): Either[GeometryError, Grid[F, D]] =
    create(
      GridId.fresh(),
      frame,
      shape.iterator.toVector,
      indexToFrame
    )

  def restore[D <: Dim](
      record: GridRecord,
      frame: Frame[D],
      registry: Registry
  )(using
      dimension: Dimension[D]
  ): Either[GeometryError, Grid[frame.type, D]] =
    if record.spatialRank != dimension.rank then
      Left(GeometryError.DimensionMismatch(dimension.rank, record.spatialRank))
    else if record.frameId != frame.id then
      Left(GeometryError.FrameMismatch(record.frameId, frame.id))
    else
      for
        affine <- Affine.fromRowMajor[D](
          record.indexToFrameRowMajor,
          record.affineTolerance
        )
        candidate <- create[D, frame.type](
          record.id,
          frame,
          record.shape,
          affine
        )
        restored <- restoreRegistered(record, candidate, registry)
      yield restored

  def align[D <: Dim, LF <: Frame[D], RF <: Frame[D]](
      left: Grid[LF, D],
      right: Grid[RF, D]
  ): Either[GeometryError, GridAlignment[D, LF, RF]] =
    GridAlignment.check(left, right)

  private def create[D <: Dim, F <: Frame[D]](
      id: GridId,
      frame: F,
      shape: Vector[Int],
      indexToFrame: Affine[D]
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
              id,
              frame,
              shape,
              indexToFrame,
              dimension,
              new RuntimeToken()
            )
          )

  private def register[D <: Dim, F <: Frame[D]](
      grid: Grid[F, D],
      registry: Registry
  ): Either[GeometryError, Unit] =
    registry.entries.get(grid.id) match
      case None =>
        registry.entries.update(
          grid.id,
          RegistryEntry(grid.record, grid.frame, grid)
        )
        Right(())
      case Some(entry)
          if entry.record == grid.record &&
            entry.frame.sameRuntimeOwnerAs(grid.frame) &&
            (entry.value eq grid) =>
        Right(())
      case Some(entry) if entry.record != grid.record =>
        Left(GeometryError.GridRestoreMetadataConflict(grid.id))
      case Some(entry) if !entry.frame.sameRuntimeOwnerAs(grid.frame) =>
        Left(
          GeometryError.GridRestoreFrameOwnerConflict(
            grid.id,
            grid.frame.id
          )
        )
      case Some(_) =>
        Left(GeometryError.GridRestoreDuplicateOwner(grid.id))

  private def restoreRegistered[D <: Dim, F <: Frame[D]](
      record: GridRecord,
      candidate: Grid[F, D],
      registry: Registry
  ): Either[GeometryError, Grid[F, D]] =
    registry.entries.get(record.id) match
      case None =>
        registry.entries.update(
          record.id,
          RegistryEntry(record, candidate.frame, candidate)
        )
        Right(candidate)
      case Some(entry) if entry.record != record =>
        Left(GeometryError.GridRestoreMetadataConflict(record.id))
      case Some(entry)
          if !entry.frame.sameRuntimeOwnerAs(candidate.frame) =>
        Left(
          GeometryError.GridRestoreFrameOwnerConflict(
            record.id,
            candidate.frame.id
          )
        )
      case Some(entry) =>
        // F is the exact candidate frame type. Equal live frame ownership plus
        // the checked record make recovery of the registered grid sound.
        Right(entry.value.asInstanceOf[Grid[F, D]])

type GridRegistry = Grid.Registry

object GridRegistry:
  def empty: GridRegistry =
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
      Point.fromVector(right.frame, point.coordinates)
    else Left(left.frame.mismatchWith(point.frame))

  def pointToLeft(
      point: Point[RF, D]
  )(using Dimension[D]): Either[GeometryError, Point[LF, D]] =
    if point.belongsTo(right.frame) then
      Point.fromVector(left.frame, point.coordinates)
    else Left(right.frame.mismatchWith(point.frame))

object GridAlignment:
  def check[D <: Dim, LF <: Frame[D], RF <: Frame[D]](
      left: Grid[LF, D],
      right: Grid[RF, D]
  ): Either[GeometryError, GridAlignment[D, LF, RF]] =
    if left.id != right.id then
      Left(GeometryError.GridMismatch(left.id, right.id))
    else if left.record != right.record then
      Left(GeometryError.GridRestoreMetadataConflict(left.id))
    else
      Frame.align(left.frame, right.frame).map(_ =>
        new GridAlignment(left, right)
      )
