package image4s

import ravel.AnyRank
import ravel.CanDropAxis
import ravel.DropAxis

/** Zero-copy evidence that an image has exactly one time axis.
  *
  * This wrapper owns no samples. It retains the original [[Sampled]] value and the checked axis
  * location.
  */
final class TimeSeriesView[
    S <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private (
    val image: Sampled[S, A, Sem, R],
    val axisIndex: Int,
    val axis: Axis
):
  def at(index: Int)(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[image.sampleSpace.F, image.sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    image.selectNonSpatial(axisIndex, index)

object TimeSeriesView:
  def from[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R]
  ): Either[ImageError, TimeSeriesView[S, A, Sem, R]] =
    image.nonSpatialAxes.uniqueIndexOf(AxisKind.Time).map { index =>
      new TimeSeriesView(image, index, image.nonSpatialAxes.values(index))
    }

/** Zero-copy evidence that an image has exactly one declared component axis.
  *
  * The caller supplies the axis kind whose uniqueness must be established. This wrapper proves only
  * axis structure. It deliberately says nothing about how values transform under changes of
  * physical frame.
  */
final class ComponentAxisView[
    S <: SampleSpace[?, ?],
    A,
    Sem,
    R <: AnyRank
] private (
    val image: Sampled[S, A, Sem, R],
    val axisIndex: Int,
    val axis: Axis
):
  def at(index: Int)(using
      CanDropAxis[R]
  ): Either[
    ImageError,
    Sampled[
      ? <: SampleSpace[image.sampleSpace.F, image.sampleSpace.D],
      A,
      Sem,
      DropAxis[R]
    ]
  ] =
    image.selectNonSpatial(axisIndex, index)

object ComponentAxisView:
  def from[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      kind: AxisKind
  ): Either[ImageError, ComponentAxisView[S, A, Sem, R]] =
    image.nonSpatialAxes.uniqueIndexOf(kind).map { index =>
      new ComponentAxisView(
        image,
        index,
        image.nonSpatialAxes.values(index)
      )
    }
