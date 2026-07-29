package image4s

import image4s.geometry.Dim
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Grid

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

  /** Recover a statically D2 sampling space at an existential boundary.
    *
    * Frame identity is retained; only its path-dependent singleton type is
    * widened to `Frame[D2]`.
    */
  final def requireD2: Either[
    ImageError,
    SampleSpace[Frame[D2], D2]
  ] =
    SampleSpace.requireD2(this)

  /** Recover a statically D3 sampling space at an existential boundary.
    *
    * Frame identity is retained; only its path-dependent singleton type is
    * widened to `Frame[D3]`.
    */
  final def requireD3: Either[
    ImageError,
    SampleSpace[Frame[D3], D3]
  ] =
    SampleSpace.requireD3(this)

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

  override def equals(other: Any): Boolean =
    other match
      case that: SomeSampleSpace =>
        (this eq that) ||
          (
            grid.shape == that.grid.shape &&
              grid.indexToFrame.rowMajor == that.grid.indexToFrame.rowMajor &&
              axisRecords == SampleSpace.axisRecords(that.nonSpatialAxes)
          )
      case _ => false

  override def hashCode(): Int =
    var hash = grid.shape.hashCode()
    hash = 31 * hash + grid.indexToFrame.rowMajor.hashCode()
    31 * hash + axisRecords.hashCode()

  override def toString: String =
    s"SampleSpace(spatialShape=${grid.shape}, nonSpatialAxes=$axisRecords)"

  private def axisRecords: Vector[(String, Int, AxisKind)] =
    SampleSpace.axisRecords(nonSpatialAxes)

object SampleSpace:
  def create[F <: Frame[D], D <: Dim](
      grid: Grid[F, D],
      nonSpatialAxes: NonSpatialAxes
  ): SampleSpace[F, D] =
    new SampleSpace(grid, nonSpatialAxes)

  private def axisRecords(
      axes: NonSpatialAxes
  ): Vector[(String, Int, AxisKind)] =
    axes.values.map(axis => (axis.name.value, axis.extent, axis.kind))

  private[image4s] def requireD2(
      space: SomeSampleSpace
  ): Either[ImageError, SampleSpace[Frame[D2], D2]] =
    if space.spatialRank == 2 then
      // SampleSpace is immutable. The checked dimension refinement and frame
      // widening preserve the same live Grid and Frame owners without a copy.
      Right(space.typed.asInstanceOf[SampleSpace[Frame[D2], D2]])
    else
      Left(ImageError.SpatialDimensionMismatch(2, space.spatialRank))

  private[image4s] def requireD3(
      space: SomeSampleSpace
  ): Either[ImageError, SampleSpace[Frame[D3], D3]] =
    if space.spatialRank == 3 then
      // SampleSpace is immutable. The checked dimension refinement and frame
      // widening preserve the same live Grid and Frame owners without a copy.
      Right(space.typed.asInstanceOf[SampleSpace[Frame[D3], D3]])
    else
      Left(ImageError.SpatialDimensionMismatch(3, space.spatialRank))
