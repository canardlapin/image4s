package image4s.ops

import image4s.geometry.Dim
import image4s.geometry.Dimension

/** One-dimensional filter factor with an explicit anchor index. */
final case class AxisKernel[A] private (
    weights: Vector[A],
    anchor: Int
):
  def length: Int =
    weights.length

object AxisKernel:
  /** Odd-length centered kernel; anchor is the unique center. */
  def centered[A](
      weights: IterableOnce[A]
  ): Either[OpError, AxisKernel[A]] =
    val copied = weights.iterator.toVector
    if copied.isEmpty then Left(OpError.InvalidKernel("axis kernel must be non-empty"))
    else if copied.length % 2 == 0 then
      Left(
        OpError.InvalidKernel(
          "even-length axis kernels require an explicit anchor"
        )
      )
    else Right(new AxisKernel(copied, copied.length / 2))

  def create[A](
      weights: IterableOnce[A],
      anchor: Int
  ): Either[OpError, AxisKernel[A]] =
    val copied = weights.iterator.toVector
    if copied.isEmpty then Left(OpError.InvalidKernel("axis kernel must be non-empty"))
    else if anchor < 0 || anchor >= copied.length then
      Left(
        OpError.InvalidKernel(
          s"anchor $anchor is outside kernel length ${copied.length}"
        )
      )
    else Right(new AxisKernel(copied, anchor))

/** Linear filter kernel over a spatial dimension. */
sealed trait Kernel[D <: Dim, A]:
  def support: Support[D]

object Kernel:
  final class Dense[D <: Dim, A] private[ops] (
      val support: Support[D],
      val weights: Vector[A]
  ) extends Kernel[D, A]

  final class Sparse[D <: Dim, A] private[ops] (
      val support: Support[D],
      val weights: Vector[A]
  ) extends Kernel[D, A]

  final class Separable[D <: Dim, A] private[ops] (
      val axes: Vector[AxisKernel[A]],
      val support: Support[D]
  ) extends Kernel[D, A]

  def dense[D <: Dim, A](
      support: Support[D],
      weights: IterableOnce[A]
  ): Either[OpError, Dense[D, A]] =
    val copied = weights.iterator.toVector
    if copied.length != support.size then
      Left(
        OpError.InvalidKernel(
          s"dense kernel has ${copied.length} weights for support size ${support.size}"
        )
      )
    else Right(new Dense(support, copied))

  def sparse[D <: Dim, A](
      support: Support[D],
      weights: IterableOnce[A]
  ): Either[OpError, Sparse[D, A]] =
    val copied = weights.iterator.toVector
    if copied.length != support.size then
      Left(
        OpError.InvalidKernel(
          s"sparse kernel has ${copied.length} weights for support size ${support.size}"
        )
      )
    else Right(new Sparse(support, copied))

  def separable[D <: Dim, A](
      axes: IterableOnce[AxisKernel[A]]
  )(using dimension: Dimension[D]): Either[OpError, Separable[D, A]] =
    val copied = axes.iterator.toVector
    if copied.length != dimension.rank then
      Left(
        OpError.InvalidKernel(
          s"separable kernel has ${copied.length} factors for spatial rank ${dimension.rank}"
        )
      )
    else
      for support <- separableSupport(copied)
      yield new Separable(copied, support)

  private def separableSupport[D <: Dim, A](
      axes: Vector[AxisKernel[A]]
  )(using dimension: Dimension[D]): Either[OpError, Support[D]] =
    val ranges =
      axes.map { axis =>
        val left = axis.anchor
        val right = axis.length - axis.anchor - 1
        (-left to right).toVector
      }
    val offsets =
      cartesian(ranges).map { coords =>
        Offset.unsafe[D](coords)
      }
    Support.create(offsets)

  private def cartesian(
      ranges: Vector[Vector[Int]]
  ): Vector[Vector[Int]] =
    ranges.foldLeft(Vector(Vector.empty[Int])) { (acc, range) =>
      for
        prefix <- acc
        value <- range
      yield prefix :+ value
    }

/** Named linear neighborhood operations. Correlation and convolution are distinct; they are never a
  * Boolean flag.
  *
  * Correlation: `(K ★ X)(p) = Σ_δ K(δ) X(p+δ)` Convolution: `(K ∗ X)(p) = Σ_δ K(δ) X(p-δ)`
  */
enum LinearNeighborhoodOp derives CanEqual:
  case Correlation, Convolution

final case class Correlation[D <: Dim, A](
    kernel: Kernel[D, A],
    extent: FilterExtent[A]
):
  def op: LinearNeighborhoodOp =
    LinearNeighborhoodOp.Correlation

final case class Convolution[D <: Dim, A](
    kernel: Kernel[D, A],
    extent: FilterExtent[A]
):
  def op: LinearNeighborhoodOp =
    LinearNeighborhoodOp.Convolution
