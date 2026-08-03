package image4s

import scala.annotation.unused
import ravel.IntegralDType
import ravel.NDArray
import ravel.Rank
import ravel.UInt16
import ravel.UInt8

/** Standard semantics for values that admit linear interpolation.
  *
  * This tag describes field meaning, not storage dtype: a `ContinuousImage[..., Byte, ...]` may
  * still represent a continuously sampled quantity stored at byte precision. Scalar versus
  * multi-component structure is expressed by axes / [[ComponentAxisView]], not by this tag.
  */
sealed trait Continuous

/** Standard semantics for categorical / label values.
  *
  * Labels are not interpolated numerically. Standard construction requires an integral element
  * type; do not treat a Boolean mask as a label image.
  */
sealed trait Categorical

/** Standard semantics for logical support masks.
  *
  * Distinct from [[Categorical]]: a mask is Boolean support, not an integer label map whose codes
  * happen to be zero and one.
  */
sealed trait Mask

/** Evidence that element type `A` may be stored with semantic tag `Sem`.
  *
  * This is the library's admissibility witness for image construction. `Sem` is intentionally
  * unbounded so downstream libraries can define their own semantic tags without changing image4s.
  * Standard tags are gated: continuous values need a linear codomain, categorical labels need an
  * integral dtype, and masks are Boolean-only.
  */
trait ValueSemantics[A, Sem]

object ValueSemantics:
  given continuous[A](using
      @unused values: LinearInterpolable[A]
  ): ValueSemantics[A, Continuous] with {}

  given categorical[A](using
      @unused integral: IntegralDType[A]
  ): ValueSemantics[A, Categorical] with {}

  given mask: ValueSemantics[Boolean, Mask] with {}

/** Primitive-to-Double widening required by linear interpolation.
  *
  * Interpolation always accumulates into an explicit Float or Double output. Integer storage is
  * therefore admissible as an input but is never silently truncated back into its source dtype.
  */
trait LinearInterpolable[A]:
  def toDouble(value: A): Double

  private[image4s] def read2(
      data: NDArray[A, Rank[2]],
      i: Int,
      j: Int
  ): Double =
    toDouble(data(i, j))

  private[image4s] def read3(
      data: NDArray[A, Rank[3]],
      i: Int,
      j: Int,
      k: Int
  ): Double =
    toDouble(data(i, j, k))

object LinearInterpolable:
  given LinearInterpolable[Double] with
    def toDouble(value: Double): Double =
      value

    override private[image4s] def read2(
        data: NDArray[Double, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j)

    override private[image4s] def read3(
        data: NDArray[Double, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k)

  given LinearInterpolable[Float] with
    def toDouble(value: Float): Double =
      value.toDouble

    override private[image4s] def read2(
        data: NDArray[Float, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toDouble

    override private[image4s] def read3(
        data: NDArray[Float, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toDouble

  given LinearInterpolable[Byte] with
    def toDouble(value: Byte): Double =
      value.toDouble

    override private[image4s] def read2(
        data: NDArray[Byte, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toDouble

    override private[image4s] def read3(
        data: NDArray[Byte, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toDouble

  given LinearInterpolable[UInt8] with
    def toDouble(value: UInt8): Double =
      value.toInt.toDouble

    override private[image4s] def read2(
        data: NDArray[UInt8, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toInt.toDouble

    override private[image4s] def read3(
        data: NDArray[UInt8, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toInt.toDouble

  given LinearInterpolable[Short] with
    def toDouble(value: Short): Double =
      value.toDouble

    override private[image4s] def read2(
        data: NDArray[Short, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toDouble

    override private[image4s] def read3(
        data: NDArray[Short, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toDouble

  given LinearInterpolable[UInt16] with
    def toDouble(value: UInt16): Double =
      value.toInt.toDouble

    override private[image4s] def read2(
        data: NDArray[UInt16, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toInt.toDouble

    override private[image4s] def read3(
        data: NDArray[UInt16, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toInt.toDouble

  given LinearInterpolable[Int] with
    def toDouble(value: Int): Double =
      value.toDouble

    override private[image4s] def read2(
        data: NDArray[Int, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toDouble

    override private[image4s] def read3(
        data: NDArray[Int, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toDouble

  given LinearInterpolable[Long] with
    def toDouble(value: Long): Double =
      value.toDouble

    override private[image4s] def read2(
        data: NDArray[Long, Rank[2]],
        i: Int,
        j: Int
    ): Double =
      data(i, j).toDouble

    override private[image4s] def read3(
        data: NDArray[Long, Rank[3]],
        i: Int,
        j: Int,
        k: Int
    ): Double =
      data(i, j, k).toDouble

/** Evidence that linear interpolation is legal for this semantic tag.
  *
  * Downstream semantic tags opt in explicitly by defining their own instance.
  */
trait LinearSampling[A, Sem]:
  def interpolation: LinearInterpolable[A]

object LinearSampling:
  given continuous[A](using
      values: LinearInterpolable[A]
  ): LinearSampling[A, Continuous] with
    val interpolation: LinearInterpolable[A] =
      values

/** Explicit floating output representation for linear sampling.
  *
  * This type is deliberately closed: reference linear interpolation accumulates in Double and may
  * return only Float or Double. It never performs implicit byte or short arithmetic.
  */
sealed trait LinearOutput[A]:
  private[image4s] def fromDouble(value: Double): A

object LinearOutput:
  given float: LinearOutput[Float] with
    private[image4s] def fromDouble(value: Double): Float =
      value.toFloat

  given double: LinearOutput[Double] with
    private[image4s] def fromDouble(value: Double): Double =
      value

/** Evidence that nearest-neighbour sampling may return `A` unchanged. */
trait NearestInterpolable[A]

object NearestInterpolable:
  given [A]: NearestInterpolable[A] with {}
