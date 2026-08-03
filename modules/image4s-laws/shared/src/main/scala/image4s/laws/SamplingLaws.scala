package image4s.laws

import image4s.BoundaryPolicy
import image4s.Continuous
import image4s.ImageError
import image4s.NearestInterpolable
import image4s.PartialWeight.value
import image4s.SampleSpace
import image4s.Sampled
import image4s.Validity
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.LatticeIndex
import image4s.geometry.Point
import image4s.reference.ReferenceSampler
import ravel.AnyRank

/** Reusable laws for the allocation-tolerant reference sampler. */
object SamplingLaws:
  def nearestAtIntegerIsExact[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      index: LatticeIndex[D],
      nonSpatialIndex: Vector[Int] = Vector.empty
  )(
      equal: (A, A) => Boolean
  )(using
      Dimension[D],
      NearestInterpolable[A]
  ): Either[ImageError, Boolean] =
    for
      expected <- image.valueAt(index.values, nonSpatialIndex)
      point <- image.grid
        .pointAt(index)
        .left
        .map(ImageError.Geometry.apply)
      actual <- ReferenceSampler.nearest(
        image,
        point,
        nonSpatialIndex
      )
    yield actual.validity == Validity.Full &&
      equal(actual.value, expected)

  def linearReproducesExpected[
      F <: Frame[D],
      D <: Dim,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, Double, Continuous, R],
      point: Point[F, D],
      expected: Double,
      tolerance: Double,
      boundary: BoundaryPolicy[Double] = BoundaryPolicy.Reject
  )(using Dimension[D]): Either[ImageError, Boolean] =
    ReferenceSampler
      .linearToDouble(image, point, boundary = boundary)
      .map(result => math.abs(result.value - expected) <= tolerance)

  def validityWeightIsBounded(validity: Validity): Boolean =
    validity match
      case Validity.Full | Validity.Outside =>
        true
      case Validity.Partial(weight) =>
        weight.value > 0.0 && weight.value < 1.0
