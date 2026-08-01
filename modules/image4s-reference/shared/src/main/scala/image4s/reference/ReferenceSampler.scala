package image4s.reference

import image4s.BoundaryPolicy
import image4s.ImageError
import image4s.LinearSampling
import image4s.NearestInterpolable
import image4s.PartialWeight
import image4s.Sample
import image4s.SampleSpace
import image4s.Sampled
import image4s.Validity
import ravel.AnyRank
import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.Frame
import image4s.geometry.Point

/**
 * Small, allocation-tolerant correctness oracle for sampling semantics.
 *
 * This API deliberately does not compile or cache a resampling plan. Production
 * resampling lives in `reframe4s-resample` after its Ravel capability gate.
 */
object ReferenceSampler:
  def nearest[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[F, D],
      nonSpatialIndex: Vector[Int] = Vector.empty,
      boundary: BoundaryPolicy[A] = BoundaryPolicy.Reject
  )(using
      Dimension[D],
      NearestInterpolable[A]
  ): Either[ImageError, Sample[A]] =
    nearestChecked(image, point, nonSpatialIndex, boundary)

  def nearestChecked[
      IF <: Frame[D],
      PF <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[IF, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[PF, D],
      nonSpatialIndex: Vector[Int] = Vector.empty,
      boundary: BoundaryPolicy[A] = BoundaryPolicy.Reject
  )(using
      Dimension[D],
      NearestInterpolable[A]
  ): Either[ImageError, Sample[A]] =
    for
      _ <- image.validateNonSpatialIndex(nonSpatialIndex)
      rebound <- rebindToImage(image, point)
      continuous <-
        image.grid
          .continuousIndexOf(rebound)
          .left
          .map(ImageError.Geometry.apply)
      index =
        continuous.values.map(value => math.floor(value + 0.5).toInt)
      sampled <-
        valueOrBoundary(
          image,
          index,
          nonSpatialIndex,
          continuous.values,
          boundary
        )
    yield sampled

  def linear[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[F, D],
      nonSpatialIndex: Vector[Int] = Vector.empty,
      boundary: BoundaryPolicy[A] = BoundaryPolicy.Reject
  )(using
      Dimension[D],
      LinearSampling[A, Sem]
  ): Either[ImageError, Sample[A]] =
    linearChecked(image, point, nonSpatialIndex, boundary)

  def linearChecked[
      IF <: Frame[D],
      PF <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[IF, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[PF, D],
      nonSpatialIndex: Vector[Int] = Vector.empty,
      boundary: BoundaryPolicy[A] = BoundaryPolicy.Reject
  )(using
      dimension: Dimension[D],
      linear: LinearSampling[A, Sem]
  ): Either[ImageError, Sample[A]] =
    for
      _ <- image.validateNonSpatialIndex(nonSpatialIndex)
      rebound <- rebindToImage(image, point)
      continuous <-
        image.grid
          .continuousIndexOf(rebound)
          .left
          .map(ImageError.Geometry.apply)
      sampled <- interpolateLinear(
        image,
        continuous.values,
        nonSpatialIndex,
        boundary
      )
    yield sampled

  private def rebindToImage[
      IF <: Frame[D],
      PF <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[IF, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[PF, D]
  )(using Dimension[D]): Either[ImageError, Point[IF, D]] =
    Frame
      .alignOwners[D, IF, PF](image.frame, point.frame)
      .left
      .map(ImageError.Geometry.apply)
      .flatMap(
        _.pointToLeft(point)
          .left
          .map(ImageError.Geometry.apply)
      )

  private def valueOrBoundary[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      spatialIndex: Vector[Int],
      nonSpatialIndex: Vector[Int],
      continuousIndex: Vector[Double],
      boundary: BoundaryPolicy[A]
  ): Either[ImageError, Sample[A]] =
    if contains(image.grid.shape, spatialIndex) then
      image
        .valueAt(spatialIndex, nonSpatialIndex)
        .map(value => Sample(value, Validity.Full))
    else
      boundary match
        case BoundaryPolicy.Reject =>
          Left(ImageError.OutsideGrid(continuousIndex))
        case BoundaryPolicy.Constant(value) =>
          Right(Sample(value, Validity.Outside))

  private def interpolateLinear[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      continuousIndex: Vector[Double],
      nonSpatialIndex: Vector[Int],
      boundary: BoundaryPolicy[A]
  )(using
      dimension: Dimension[D],
      linear: LinearSampling[A, Sem]
  ): Either[ImageError, Sample[A]] =
    val lower =
      continuousIndex.map(math.floor(_).toInt)
    val fraction =
      continuousIndex
        .zip(lower)
        .map { case (coordinate, base) => coordinate - base.toDouble }
    val cornerCount = 1 << dimension.rank
    var corner = 0
    var total = linear.interpolation.zero
    var insideWeight = 0.0
    var rejected = false
    var failure: Option[ImageError] = None
    while corner < cornerCount && failure.isEmpty do
      val index =
        Vector.tabulate(dimension.rank) { axis =>
          lower(axis) + ((corner >>> axis) & 1)
        }
      var weight = 1.0
      var axis = 0
      while axis < dimension.rank do
        val factor =
          if ((corner >>> axis) & 1) == 1 then fraction(axis)
          else 1.0 - fraction(axis)
        weight *= factor
        axis += 1
      if weight > 0.0 then
        if contains(image.grid.shape, index) then
          image.valueAt(index, nonSpatialIndex) match
            case Right(value) =>
              total =
                linear.interpolation.addScaled(total, value, weight)
              insideWeight += weight
            case Left(error) =>
              failure = Some(error)
        else
          boundary match
            case BoundaryPolicy.Reject =>
              rejected = true
            case BoundaryPolicy.Constant(value) =>
              total =
                linear.interpolation.addScaled(total, value, weight)
      corner += 1

    failure match
      case Some(error) => Left(error)
      case None if rejected =>
        Left(ImageError.OutsideGrid(continuousIndex))
      case None if insideWeight >= 1.0 - 1e-12 =>
        Right(Sample(total, Validity.Full))
      case None if insideWeight <= 1e-12 =>
        Right(Sample(total, Validity.Outside))
      case None =>
        PartialWeight
          .from(insideWeight)
          .map(weight => Sample(total, Validity.Partial(weight)))

  private def contains(
      shape: Vector[Int],
      index: Vector[Int]
  ): Boolean =
    index.length == shape.length &&
      index.indices.forall(axis =>
        index(axis) >= 0 && index(axis) < shape(axis)
      )
