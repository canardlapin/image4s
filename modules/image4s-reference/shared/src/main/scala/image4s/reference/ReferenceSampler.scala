package image4s.reference

import image4s.BoundaryPolicy
import image4s.ImageError
import image4s.LinearOutput
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

/** Small, allocation-tolerant correctness oracle for sampling semantics.
  *
  * This API deliberately does not compile or cache a resampling plan. Production resampling lives
  * in `reframe4s-resample` after its Ravel capability gate.
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

  /** Linearly sample into a Float result. */
  def linearToFloat[
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
  ): Either[ImageError, Sample[Float]] =
    linearToOutput(image, point, nonSpatialIndex, boundary, LinearOutput.float)

  /** Linearly sample into a Double result. */
  def linearToDouble[
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
  ): Either[ImageError, Sample[Double]] =
    linearToOutput(image, point, nonSpatialIndex, boundary, LinearOutput.double)

  /** Frame-rebinding Float-output variant of [[linearToFloat]]. */
  def linearToFloatChecked[
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
      LinearSampling[A, Sem]
  ): Either[ImageError, Sample[Float]] =
    linearToOutputChecked(
      image,
      point,
      nonSpatialIndex,
      boundary,
      LinearOutput.float
    )

  /** Frame-rebinding Double-output variant of [[linearToDouble]]. */
  def linearToDoubleChecked[
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
  ): Either[ImageError, Sample[Double]] =
    linearToOutputChecked(
      image,
      point,
      nonSpatialIndex,
      boundary,
      LinearOutput.double
    )

  private def linearToOutput[
      F <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank,
      B
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[F, D],
      nonSpatialIndex: Vector[Int],
      boundary: BoundaryPolicy[A],
      output: LinearOutput[B]
  )(using
      dimension: Dimension[D],
      linear: LinearSampling[A, Sem]
  ): Either[ImageError, Sample[B]] =
    linearToOutputChecked(image, point, nonSpatialIndex, boundary, output)

  private def linearToOutputChecked[
      IF <: Frame[D],
      PF <: Frame[D],
      D <: Dim,
      A,
      Sem,
      S <: SampleSpace[IF, D],
      R <: AnyRank,
      B
  ](
      image: Sampled[S, A, Sem, R],
      point: Point[PF, D],
      nonSpatialIndex: Vector[Int],
      boundary: BoundaryPolicy[A],
      output: LinearOutput[B]
  )(using
      dimension: Dimension[D],
      linear: LinearSampling[A, Sem]
  ): Either[ImageError, Sample[B]] =
    for
      _ <- image.validateNonSpatialIndex(nonSpatialIndex)
      rebound <- rebindToImage(image, point)
      continuous <-
        image.grid
          .continuousIndexOf(rebound)
          .left
          .map(ImageError.Geometry.apply)
      sampled <- interpolateLinearTo(
        image,
        continuous.values,
        nonSpatialIndex,
        boundary,
        output
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
        _.pointToLeft(point).left
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

  private def interpolateLinearTo[
      F <: Frame[D],
      D <: Dim,
      A,
      B,
      Sem,
      S <: SampleSpace[F, D],
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      continuousIndex: Vector[Double],
      nonSpatialIndex: Vector[Int],
      boundary: BoundaryPolicy[A],
      output: LinearOutput[B]
  )(using
      dimension: Dimension[D],
      linear: LinearSampling[A, Sem]
  ): Either[ImageError, Sample[B]] =
    val lower =
      continuousIndex.map(math.floor(_).toInt)
    val fraction =
      continuousIndex
        .zip(lower)
        .map { case (coordinate, base) => coordinate - base.toDouble }
    val cornerCount = 1 << dimension.rank
    var corner = 0
    var total = 0.0
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
              total += linear.interpolation.toDouble(value) * weight
              insideWeight += weight
            case Left(error) =>
              failure = Some(error)
        else
          boundary match
            case BoundaryPolicy.Reject =>
              rejected = true
            case BoundaryPolicy.Constant(value) =>
              total += linear.interpolation.toDouble(value) * weight
      corner += 1

    failure match
      case Some(error) => Left(error)
      case None if rejected =>
        Left(ImageError.OutsideGrid(continuousIndex))
      case None if insideWeight >= 1.0 - 1e-12 =>
        Right(Sample(output.fromDouble(total), Validity.Full))
      case None if insideWeight <= 1e-12 =>
        Right(Sample(output.fromDouble(total), Validity.Outside))
      case None =>
        PartialWeight
          .from(insideWeight)
          .map(weight => Sample(output.fromDouble(total), Validity.Partial(weight)))

  private def contains(
      shape: Vector[Int],
      index: Vector[Int]
  ): Boolean =
    index.length == shape.length &&
      index.indices.forall(axis => index(axis) >= 0 && index(axis) < shape(axis))
