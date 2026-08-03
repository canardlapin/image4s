package image4s

import image4s.geometry.Dim
import image4s.geometry.Frame
import image4s.geometry.Grid

/** Result of comparing persistent sample-space identity.
  *
  * Ephemeral owners are reported explicitly rather than treated as unequal persistent identities.
  */
enum PersistentSpaceComparison derives CanEqual:
  case Same
  case Different
  case LeftEphemeral
  case RightEphemeral
  case BothEphemeral

/** What image4s can prove about two Ravel storage owners. */
enum StorageSharing derives CanEqual:
  case SameArrayObject
  case Unknown

/** Reusable evidence that two complete spaces sample the same points and ordered non-spatial
  * coordinates exactly.
  */
final class SamplingAlignment[
    L <: SampleSpace[?, ?],
    R <: SampleSpace[?, ?]
] private[image4s] (
    val left: L,
    val right: R
):
  def reverse: SamplingAlignment[R, L] =
    new SamplingAlignment(right, left)

  def andThen[T <: SampleSpace[?, ?]](
      next: SamplingAlignment[R, T]
  ): SamplingAlignment[L, T] =
    new SamplingAlignment(left, next.right)

object SamplingAlignment:
  private[image4s] def widen[
      L <: SampleSpace[?, ?],
      R <: SampleSpace[?, ?],
      ExactL <: L,
      ExactR <: R
  ](
      alignment: SamplingAlignment[ExactL, ExactR]
  ): SamplingAlignment[L, R] =
    new SamplingAlignment(alignment.left, alignment.right)

  def identity[S <: SampleSpace[?, ?]](
      space: S
  ): SamplingAlignment[S, S] =
    new SamplingAlignment(space, space)

  def exact[
      D <: Dim,
      LF <: Frame[D],
      RF <: Frame[D]
  ](
      left: SampleSpace[LF, D],
      right: SampleSpace[RF, D]
  ): Either[
    ImageError,
    SamplingAlignment[left.type, right.type]
  ] =
    for
      _ <- Grid
        .exactCongruence(left.grid, right.grid)
        .left
        .map(ImageError.Geometry.apply)
      _ <-
        Either.cond(
          left.nonSpatialAxes.records == right.nonSpatialAxes.records,
          (),
          ImageError.NonSpatialSamplingMismatch(
            left.nonSpatialAxes.records,
            right.nonSpatialAxes.records
          )
        )
    yield new SamplingAlignment(left, right)

/** Checked approximate geometric congruence. Non-spatial sampling remains exact; approximation
  * applies only to the grid affine.
  */
final class ApproximateSamplingCongruence[
    L <: SampleSpace[?, ?],
    R <: SampleSpace[?, ?]
] private[image4s] (
    val left: L,
    val right: R,
    val tolerance: Double
)

object ApproximateSamplingCongruence:
  def check[
      D <: Dim,
      LF <: Frame[D],
      RF <: Frame[D]
  ](
      left: SampleSpace[LF, D],
      right: SampleSpace[RF, D],
      tolerance: Double
  ): Either[
    ImageError,
    ApproximateSamplingCongruence[left.type, right.type]
  ] =
    for
      _ <- Grid
        .approximateCongruence(left.grid, right.grid, tolerance)
        .left
        .map(ImageError.Geometry.apply)
      _ <-
        Either.cond(
          left.nonSpatialAxes.records == right.nonSpatialAxes.records,
          (),
          ImageError.NonSpatialSamplingMismatch(
            left.nonSpatialAxes.records,
            right.nonSpatialAxes.records
          )
        )
    yield new ApproximateSamplingCongruence(left, right, tolerance)
