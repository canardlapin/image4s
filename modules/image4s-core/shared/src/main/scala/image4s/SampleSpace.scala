package image4s

import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid
import image4s.geometry.GridRecord

/** Neutral persistent record for one complete sampling space.
  *
  * The grid record carries persistent physical identity. Axis records carry
  * ordered non-spatial sampling coordinates but no live owner identity.
  */
final case class SampleSpaceRecord(
    grid: GridRecord,
    nonSpatialAxes: Vector[AxisRecord]
) derives CanEqual

/** Canonical sampling geometry shared by images with the same spatial grid
  * and ordered non-spatial axes.
  *
  * This is also the runtime-erased geometry view: the abstract `D` and `F`
  * members retain the precise dimension and frame owner without introducing
  * a second wrapper.
  */
sealed trait SomeSampleSpace:
  type D <: Dim
  type F <: Frame[D]

  val dimension: Dimension[D]
  val grid: Grid[F, D]
  val nonSpatialAxes: NonSpatialAxes
  val typed: SampleSpace[F, D]

  final def spatialRank: Int =
    dimension.rank

  final def logicalShape: Vector[Int] =
    grid.shape ++ nonSpatialAxes.shape

final class SampleSpace[
    F0 <: Frame[D0],
    D0 <: Dim
] private[image4s] (
    val grid: Grid[F0, D0],
    val nonSpatialAxes: NonSpatialAxes
) extends SomeSampleSpace:
  type D = D0
  type F = F0

  val dimension: Dimension[D0] =
    grid.dimension

  val typed: SampleSpace[F0, D0] =
    this

  def appendNonSpatial(
      axis: Axis
  ): Either[ImageError, SampleSpace[F0, D0]] =
    nonSpatialAxes
      .append(axis)
      .map(axes => SampleSpace.create(grid, axes))

  def removeNonSpatial(
      index: Int
  ): Either[ImageError, SampleSpace[F0, D0]] =
    nonSpatialAxes
      .remove(index)
      .map(axes => SampleSpace.create(grid, axes))

  def updateNonSpatial(
      index: Int,
      axis: Axis
  ): Either[ImageError, SampleSpace[F0, D0]] =
    nonSpatialAxes
      .updated(index, axis)
      .map(axes => SampleSpace.create(grid, axes))

  def spatialOnly: SampleSpace[F0, D0] =
    if nonSpatialAxes.size == 0 then this
    else SampleSpace.create(grid, NonSpatialAxes.empty)

  def record: Either[ImageError, SampleSpaceRecord] =
    grid.record
      .left
      .map(ImageError.Geometry.apply)
      .map(SampleSpaceRecord(_, nonSpatialAxes.records))

  def sameRuntimeSpaceAs(other: SomeSampleSpace): Boolean =
    this eq other

  final override def equals(other: Any): Boolean =
    other match
      case reference: AnyRef => this eq reference
      case _                 => false

  final override def hashCode(): Int =
    System.identityHashCode(this)

  def persistentRelationTo(
      other: SomeSampleSpace
  ): PersistentSpaceComparison =
    (record, other.typed.record) match
      case (Right(left), Right(right)) =>
        if left == right then PersistentSpaceComparison.Same
        else PersistentSpaceComparison.Different
      case (Left(_), Right(_)) =>
        PersistentSpaceComparison.LeftEphemeral
      case (Right(_), Left(_)) =>
        PersistentSpaceComparison.RightEphemeral
      case (Left(_), Left(_)) =>
        PersistentSpaceComparison.BothEphemeral

  def samePersistentSpaceAs(
      other: SomeSampleSpace
  ): Either[ImageError, Boolean] =
    persistentRelationTo(other) match
      case PersistentSpaceComparison.Same =>
        Right(true)
      case PersistentSpaceComparison.Different =>
        Right(false)
      case PersistentSpaceComparison.LeftEphemeral =>
        Left(ImageError.PersistentSampleSpaceUnavailable(true, false))
      case PersistentSpaceComparison.RightEphemeral =>
        Left(ImageError.PersistentSampleSpaceUnavailable(false, true))
      case PersistentSpaceComparison.BothEphemeral =>
        Left(ImageError.PersistentSampleSpaceUnavailable(true, true))

  def alignExact[
      RF <: Frame[D0]
  ](
      right: SampleSpace[RF, D0]
  ): Either[
    ImageError,
    SamplingAlignment[this.type, right.type]
  ] =
    SamplingAlignment.exact(this, right)

  def approximatelyCongruentTo[
      RF <: Frame[D0]
  ](
      right: SampleSpace[RF, D0],
      tolerance: Double
  ): Either[
    ImageError,
    ApproximateSamplingCongruence[this.type, right.type]
  ] =
    ApproximateSamplingCongruence.check(this, right, tolerance)

  override def toString: String =
    s"SampleSpace(spatialShape=${grid.shape}, nonSpatialAxes=$axisRecords)"

  private def axisRecords: Vector[AxisRecord] =
    SampleSpace.axisRecords(nonSpatialAxes)

object SampleSpace:
  def create[F <: Frame[D], D <: Dim](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes
  ): SampleSpace[F, D] =
    new SampleSpace(grid, nonSpatialAxes)

  def restore[F <: Frame[D], D <: Dim](
      record: SampleSpaceRecord,
      grid: Grid[F, D]
  ): Either[ImageError, SampleSpace[F, D]] =
    for
      liveGridRecord <- grid.record.left.map(ImageError.Geometry.apply)
      _ <-
        Either.cond(
          liveGridRecord == record.grid,
          (),
          ImageError.SampleSpaceGridRecordMismatch(
            record.grid,
            liveGridRecord
          )
        )
      axes <- NonSpatialAxes.fromRecords(record.nonSpatialAxes)
    yield create(grid, axes)

  private def axisRecords(
      axes: NonSpatialAxes
  ): Vector[AxisRecord] =
    axes.records
