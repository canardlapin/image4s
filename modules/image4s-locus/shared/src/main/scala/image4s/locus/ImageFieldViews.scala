package image4s.locus

import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import locus4s.Index
import locus4s.data.Field
import ravel.AnyRank
import ravel.NDArray

/** Zero-copy field view of one spatial-only image.
  *
  * The image remains the sole owner of Ravel storage. This wrapper contains only the checked
  * grid-domain bridge and an image reference.
  */
final class SpatialFieldView[
    S,
    I <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private[locus] (
    val bridge: GridDomain[?, ?, S],
    val image: Sampled[I, A, Sem, R]
) extends Field[S, A]:
  val space = bridge.space

  def sourceData: NDArray[A, R] =
    image.data

  def apply(index: Index[S]): A =
    val ordinal = index.ordinal
    bridge.grid.shape match
      case Vector(_, second) =>
        image.data(ordinal / second, ordinal % second)
      case Vector(_, second, third) =>
        val plane = second * third
        val firstIndex = ordinal / plane
        val withinPlane = ordinal % plane
        image.data(
          firstIndex,
          withinPlane / third,
          withinPlane % third
        )
      case _ =>
        // Geometry admits only D2 and D3. Keep this branch total if a future
        // dimension is added before this adapter is revised.
        val coordinates =
          bridge.coordinatesOfOrdinalUnchecked(ordinal)
        image.data.at(IArray.unsafeFromArray(coordinates.toArray))

/** Non-owning view of every non-spatial value at one voxel. */
final class VoxelSeriesView[
    I <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private[locus] (
    val image: Sampled[I, A, Sem, R],
    val spatialIndex: Vector[Int]
):
  def nonSpatialAxes: NonSpatialAxes =
    image.nonSpatialAxes

  def sourceData: NDArray[A, R] =
    image.data

  def valueAt(
      nonSpatialIndex: Vector[Int]
  ): Either[ImageError, A] =
    image.valueAt(spatialIndex, nonSpatialIndex)

/** Zero-copy voxel-indexed field whose values are non-spatial series views. */
final class SeriesFieldView[
    S,
    I <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private[locus] (
    val bridge: GridDomain[?, ?, S],
    val image: Sampled[I, A, Sem, R]
) extends Field[S, VoxelSeriesView[I, A, Sem, R]]:
  val space = bridge.space

  def sourceData: NDArray[A, R] =
    image.data

  def apply(index: Index[S]): VoxelSeriesView[I, A, Sem, R] =
    new VoxelSeriesView(
      image,
      bridge.coordinatesOfOrdinalUnchecked(index.ordinal)
    )
