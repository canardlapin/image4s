package image4s.laws

import image4s.ImageError
import image4s.LatticeMap
import image4s.SamplingAlignment
import image4s.Sampled
import image4s.SampleSpace
import image4s.ValueSemantics
import image4s.geometry.Dimension
import ravel.AnyRank
import ravel.DType

/** Explicit finite policy for floating-point image comparison. */
final class NumericTolerance private (
    val absolute: Double,
    val relative: Double,
    val equalNaN: Boolean
)

object NumericTolerance:
  def create(
      absolute: Double,
      relative: Double = 0.0,
      equalNaN: Boolean = false
  ): Either[ImageError, NumericTolerance] =
    if
      absolute.isFinite && absolute >= 0.0 &&
      relative.isFinite && relative >= 0.0
    then Right(new NumericTolerance(absolute, relative, equalNaN))
    else Left(ImageError.InvalidNumericTolerance(absolute, relative))

object ImageLaws:
  def shapeAgrees[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  ): Boolean =
    sampled.logicalShape ==
      Vector.tabulate(sampled.data.shape.rank)(sampled.data.shape.apply)

  /** Compare logical values of two images with the same static space owner. */
  def allClose[
      S <: SampleSpace[?, ?],
      Sem,
      R <: AnyRank
  ](
      left: Sampled[S, Double, Sem, R],
      right: Sampled[S, Double, Sem, R],
      tolerance: NumericTolerance
  ): Boolean =
    compareValues(left, right, tolerance)

  /** Compare logical values after consuming one checked exact alignment. */
  def allCloseAligned[
      L <: SampleSpace[?, ?],
      RSpace <: SampleSpace[?, ?],
      Sem,
      RankType <: AnyRank
  ](
      left: Sampled[L, Double, Sem, RankType],
      right: Sampled[RSpace, Double, Sem, RankType],
      alignment: SamplingAlignment[L, RSpace],
      tolerance: NumericTolerance
  ): Boolean =
    compareValues(left, right.rebind(alignment.reverse), tolerance)

  def mapIdentity[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  )(
      equal: (A, A) => Boolean
  )(using
      DType[A]
  ): Boolean =
    val mapped = sampled.mapValues(identity)
    (mapped.sampleSpace eq sampled.sampleSpace) &&
      sampled.sameValuesAs(mapped)(equal)

  def mapComposition[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R],
      first: A => A,
      second: A => A
  )(
      equal: (A, A) => Boolean
  )(using
      DType[A]
  ): Boolean =
    val sequential =
      sampled
        .mapValues(first)
        .mapValues(second)
    val composed =
      sampled.mapValues(first.andThen(second))
    sequential.sameValuesAs(composed)(equal)

  def exactViewIdentity[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  ): Boolean =
    given Dimension[sampled.sampleSpace.D] =
      sampled.sampleSpace.dimension
    LatticeMap
      .identity[sampled.sampleSpace.D](sampled.grid.shape)
      .flatMap(sampled.view)
      .exists(result =>
        (result.sampleSpace eq sampled.sampleSpace) &&
          (result.data eq sampled.data)
      )

  def exactViewComposition[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  )(
      first: LatticeMap[sampled.sampleSpace.D],
      second: LatticeMap[sampled.sampleSpace.D]
  )(
      equal: (A, A) => Boolean
  ): Boolean =
    given Dimension[sampled.sampleSpace.D] =
      sampled.sampleSpace.dimension
    val sequential =
      sampled.view(first).flatMap(_.view(second))
    val direct =
      first.followedBy(second).flatMap(sampled.view)
    (sequential, direct) match
      case (Right(left), Right(right)) =>
        left.logicalShape == right.logicalShape &&
          left.grid.indexToFrame.rowMajor ==
            right.grid.indexToFrame.rowMajor &&
          left.sameValuesAs(right)(equal)
      case _ =>
        false

  def canonicalLayoutIdempotent[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  ): Boolean =
    val canonical = sampled.canonicalLayout
    canonical.data.isCanonicalLayout &&
      (canonical.canonicalLayout eq canonical)

  def materializedCopyPreservesValues[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      sampled: Sampled[S, A, Sem, R]
  )(
      equal: (A, A) => Boolean
  ): Boolean =
    val copied = sampled.materializedCopy
    (copied.data ne sampled.data) &&
      (copied.sampleSpace eq sampled.sampleSpace) &&
      copied.sameValuesAs(sampled)(equal)

  def rebindSharesData[
      L <: SampleSpace[?, ?],
      RSpace <: SampleSpace[?, ?],
      A,
      Sem,
      RankType <: AnyRank
  ](
      sampled: Sampled[L, A, Sem, RankType],
      alignment: SamplingAlignment[L, RSpace]
  ): Boolean =
    val rebound = sampled.rebind(alignment)
    (rebound.sampleSpace eq alignment.right) &&
      (rebound.data eq sampled.data) &&
      rebound.metadata == sampled.metadata

  def alignedZipAgreesPointwise[
      L <: SampleSpace[?, ?],
      RSpace <: SampleSpace[?, ?],
      A,
      LeftSem,
      RightSem,
      C,
      OutSem,
      RankType <: AnyRank
  ](
      left: Sampled[L, A, LeftSem, RankType],
      right: Sampled[RSpace, A, RightSem, RankType],
      alignment: SamplingAlignment[L, RSpace],
      combine: (A, A) => C
  )(
      equal: (C, C) => Boolean
  )(using
      ValueSemantics[C, OutSem],
      DType[C]
  ): Boolean =
    val combined =
      left.zipWithAlignedAs[
        RSpace,
        RightSem,
        C,
        OutSem
      ](right, alignment)(combine)
    val actual = combined.data.elementsIterator
    val leftValues = left.data.elementsIterator
    val rightValues = right.data.elementsIterator
    var agrees = true
    while
      agrees &&
      actual.hasNext &&
      leftValues.hasNext &&
      rightValues.hasNext
    do
      agrees =
        equal(
          actual.next(),
          combine(leftValues.next(), rightValues.next())
        )
    agrees &&
      !actual.hasNext &&
      !leftValues.hasNext &&
      !rightValues.hasNext

  def zipPreservingAgreesPointwise[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      RankType <: AnyRank
  ](
      left: Sampled[S, A, Sem, RankType],
      right: Sampled[S, A, Sem, RankType],
      combine: (A, A) => A
  )(
      equal: (A, A) => Boolean
  )(using DType[A]): Boolean =
    val combined = left.zipWith(right)(combine)
    zipResultAgrees(left, right, combined, combine, equal)

  def alignedZipPreservingAgreesPointwise[
      L <: SampleSpace[?, ?],
      RSpace <: SampleSpace[?, ?],
      A,
      Sem,
      RankType <: AnyRank
  ](
      left: Sampled[L, A, Sem, RankType],
      right: Sampled[RSpace, A, Sem, RankType],
      alignment: SamplingAlignment[L, RSpace],
      combine: (A, A) => A
  )(
      equal: (A, A) => Boolean
  )(using DType[A]): Boolean =
    val combined =
      left.zipWithAligned(right, alignment)(combine)
    zipResultAgrees(left, right, combined, combine, equal)

  private def zipResultAgrees[
      L <: SampleSpace[?, ?],
      RSpace <: SampleSpace[?, ?],
      A,
      LeftSem,
      RightSem,
      OutSem,
      RankType <: AnyRank,
      CombinedRank <: AnyRank
  ](
      left: Sampled[L, A, LeftSem, RankType],
      right: Sampled[RSpace, A, RightSem, RankType],
      combined: Sampled[L, A, OutSem, CombinedRank],
      combine: (A, A) => A,
      equal: (A, A) => Boolean
  ): Boolean =
    val actual = combined.data.elementsIterator
    val leftValues = left.data.elementsIterator
    val rightValues = right.data.elementsIterator
    var agrees = true
    while
      agrees &&
      actual.hasNext &&
      leftValues.hasNext &&
      rightValues.hasNext
    do
      agrees =
        equal(
          actual.next(),
          combine(leftValues.next(), rightValues.next())
        )
    agrees &&
      !actual.hasNext &&
      !leftValues.hasNext &&
      !rightValues.hasNext

  private def compareValues[
      S <: SampleSpace[?, ?],
      Sem,
      R <: AnyRank
  ](
      left: Sampled[S, Double, Sem, R],
      right: Sampled[S, Double, Sem, R],
      tolerance: NumericTolerance
  ): Boolean =
    left.sameValuesAs(right)((a, b) => close(a, b, tolerance))

  private def close(
      left: Double,
      right: Double,
      tolerance: NumericTolerance
  ): Boolean =
    if left == right then true
    else if left.isNaN || right.isNaN then
      tolerance.equalNaN && left.isNaN && right.isNaN
    else if !left.isFinite || !right.isFinite then false
    else
      val difference = math.abs(left - right)
      val scale = math.max(math.abs(left), math.abs(right))
      difference <= tolerance.absolute + tolerance.relative * scale
