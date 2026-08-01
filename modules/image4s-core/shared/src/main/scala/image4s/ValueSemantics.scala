package image4s

import scala.annotation.unused
import ravel.IntegralDType

/** Standard semantics for values that admit linear interpolation.
  *
  * This tag describes field meaning, not storage dtype: a
  * `ContinuousImage[..., Byte, ...]` may still represent a continuously
  * sampled quantity stored at byte precision. Scalar versus multi-component
  * structure is expressed by axes / [[ComponentAxisView]], not by this tag.
  */
sealed trait Continuous

/** Standard semantics for categorical / label values.
  *
  * Labels are not interpolated numerically. Standard construction requires an
  * integral element type; do not treat a Boolean mask as a label image.
  */
sealed trait Categorical

/** Standard semantics for logical support masks.
  *
  * Distinct from [[Categorical]]: a mask is Boolean support, not an integer
  * label map whose codes happen to be zero and one.
  */
sealed trait Mask

/** Evidence that element type `A` may be stored with semantic tag `Sem`.
  *
  * This is the library's admissibility witness for image construction. `Sem`
  * is intentionally unbounded so downstream libraries can define their own
  * semantic tags without changing image4s. Standard tags are gated:
  * continuous values need a linear codomain, categorical labels need an
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

/** Codomain algebra required by linear interpolation. */
trait LinearInterpolable[A]:
  def zero: A
  def addScaled(accumulator: A, value: A, weight: Double): A

object LinearInterpolable:
  given LinearInterpolable[Double] with
    def zero: Double =
      0.0

    def addScaled(
        accumulator: Double,
        value: Double,
        weight: Double
    ): Double =
      accumulator + value * weight

  given LinearInterpolable[Float] with
    def zero: Float =
      0.0f

    def addScaled(
        accumulator: Float,
        value: Float,
        weight: Double
    ): Float =
      (accumulator.toDouble + value.toDouble * weight).toFloat

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

/** Evidence that nearest-neighbour sampling may return `A` unchanged. */
trait NearestInterpolable[A]

object NearestInterpolable:
  given [A]: NearestInterpolable[A] with {}
