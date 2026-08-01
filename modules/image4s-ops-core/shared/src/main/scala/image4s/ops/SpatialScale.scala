package image4s.ops

import image4s.geometry.Dim
import image4s.geometry.Dimension
import image4s.geometry.LengthUnit

/** Isotropic or anisotropic Gaussian scale in sample or frame coordinates. */
sealed trait SpatialSigma[D <: Dim] derives CanEqual

object SpatialSigma:
  final class Samples[D <: Dim] private[ops] (
      val values: Vector[Double]
  ) extends SpatialSigma[D]

  final class FrameUnits[D <: Dim] private[ops] (
      val values: Vector[Double],
      val unit: Option[LengthUnit]
  ) extends SpatialSigma[D]

  def samples[D <: Dim](
      sigma: Double
  )(using dimension: Dimension[D]): Either[OpError, Samples[D]] =
    validatePositive(sigma).map { value =>
      new Samples(Vector.fill(dimension.rank)(value))
    }

  def samples[D <: Dim](
      sigmas: IterableOnce[Double]
  )(using dimension: Dimension[D]): Either[OpError, Samples[D]] =
    val copied = sigmas.iterator.toVector
    if copied.length != dimension.rank then
      Left(
        OpError.InvalidScale(
          s"sample sigma rank ${copied.length} does not match spatial rank ${dimension.rank}"
        )
      )
    else
      copied.find(_ <= 0.0).orElse(copied.find(v => !v.isFinite)) match
        case Some(bad) =>
          Left(
            OpError.InvalidScale(
              s"sample sigma must be positive finite, got $bad"
            )
          )
        case None =>
          Right(new Samples(copied))

  def frame[D <: Dim](
      sigma: Double,
      unit: Option[LengthUnit] = None
  )(using dimension: Dimension[D]): Either[OpError, FrameUnits[D]] =
    validatePositive(sigma).map { value =>
      new FrameUnits(Vector.fill(dimension.rank)(value), unit)
    }

  def frame[D <: Dim](
      sigmas: IterableOnce[Double],
      unit: Option[LengthUnit]
  )(using dimension: Dimension[D]): Either[OpError, FrameUnits[D]] =
    val copied = sigmas.iterator.toVector
    if copied.length != dimension.rank then
      Left(
        OpError.InvalidScale(
          s"frame sigma rank ${copied.length} does not match spatial rank ${dimension.rank}"
        )
      )
    else
      copied.find(_ <= 0.0).orElse(copied.find(v => !v.isFinite)) match
        case Some(bad) =>
          Left(
            OpError.InvalidScale(
              s"frame sigma must be positive finite, got $bad"
            )
          )
        case None =>
          Right(new FrameUnits(copied, unit))

  private def validatePositive(
      sigma: Double
  ): Either[OpError, Double] =
    if !sigma.isFinite || sigma <= 0.0 then
      Left(OpError.InvalidScale(s"sigma must be positive finite, got $sigma"))
    else Right(sigma)

/** Structuring-element / neighborhood radius. */
sealed trait Radius derives CanEqual

object Radius:
  final class Samples private[ops] (val value: Int) extends Radius

  final class FrameUnits private[ops] (
      val value: Double,
      val unit: Option[LengthUnit]
  ) extends Radius

  def samples(value: Int): Either[OpError, Samples] =
    if value < 0 then
      Left(
        OpError.InvalidScale(s"sample radius must be non-negative, got $value")
      )
    else Right(new Samples(value))

  def frame(
      value: Double,
      unit: Option[LengthUnit] = None
  ): Either[OpError, FrameUnits] =
    if !value.isFinite || value < 0.0 then
      Left(
        OpError.InvalidScale(
          s"frame radius must be finite and non-negative, got $value"
        )
      )
    else Right(new FrameUnits(value, unit))
