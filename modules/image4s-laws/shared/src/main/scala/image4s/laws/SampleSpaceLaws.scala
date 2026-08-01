package image4s.laws

import image4s.SampleSpace
import image4s.SamplingAlignment

/** Reusable laws for complete sample-space ownership and alignment. */
object SampleSpaceLaws:
  def logicalShapeAgrees(
      space: SampleSpace[?, ?]
  ): Boolean =
    space.logicalShape ==
      space.grid.shape ++ space.nonSpatialAxes.shape

  def alignmentIdentity[
      S <: SampleSpace[?, ?]
  ](
      space: S
  ): Boolean =
    val alignment = SamplingAlignment.identity(space)
    (alignment.left eq space) &&
      (alignment.right eq space) &&
      (alignment.reverse.left eq space) &&
      (alignment.reverse.right eq space)

  def alignmentReverse[
      L <: SampleSpace[?, ?],
      R <: SampleSpace[?, ?]
  ](
      alignment: SamplingAlignment[L, R]
  ): Boolean =
    val roundTrip = alignment.reverse.reverse
    (roundTrip.left eq alignment.left) &&
      (roundTrip.right eq alignment.right)

  def alignmentComposition[
      L <: SampleSpace[?, ?],
      M <: SampleSpace[?, ?],
      R <: SampleSpace[?, ?]
  ](
      first: SamplingAlignment[L, M],
      second: SamplingAlignment[M, R]
  ): Boolean =
    val composed = first.andThen(second)
    (composed.left eq first.left) &&
      (composed.right eq second.right)
