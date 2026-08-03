package image4s

import ravel.AnyRank
import ravel.packed.PackedArray
import ravel.packed.PackedBitOps
import ravel.packed.PackedBits
import ravel.packed.PackedError

/** Errors raised while packing images into sub-byte codes. */
sealed trait PackedImageError derives CanEqual:
  def message: String

object PackedImageError:
  final case class Packed(error: PackedError) extends PackedImageError:
    def message: String =
      error.message

  final case class SpaceMismatch(
      leftShape: Vector[Int],
      rightShape: Vector[Int]
  ) extends PackedImageError:
    def message: String =
      s"packed operands sample different spaces: shapes ${leftShape.mkString("(", ",", ")")} " +
        s"and ${rightShape.mkString("(", ",", ")")} or different grid owners"

  final case class InvalidQuantizer(detail: String) extends PackedImageError:
    def message: String =
      detail

/** Code-to-value mapping for one packed representation.
  *
  * Encodings are total: mask and label encodings are direct code embeddings (labels outside the
  * code width are rejected while packing), and the uniform quantizer saturates to its window rather
  * than failing.
  */
sealed trait PackedEncoding[A]:
  def bits: PackedBits
  def encode(value: A): Int
  def decode(code: Int): A
  def decodeToFloat(code: Int): Float

object PackedEncoding:
  /** One-bit Boolean mask codes. */
  case object Mask extends PackedEncoding[Boolean]:
    val bits: PackedBits = PackedBits.B1

    def encode(value: Boolean): Int =
      if value then 1 else 0

    def decode(code: Int): Boolean =
      code != 0

    def decodeToFloat(code: Int): Float =
      if code != 0 then 1.0f else 0.0f

  /** Small nonnegative label codes stored verbatim. */
  final case class Labels(bits: PackedBits) extends PackedEncoding[Int]:
    def encode(value: Int): Int =
      value

    def decode(code: Int): Int =
      code

    def decodeToFloat(code: Int): Float =
      code.toFloat

  /** Uniform scalar quantizer over a closed window.
    *
    * Encoding clamps to `[lower, upper]` (saturation policy) and rounds to the nearest of `2^bits`
    * evenly spaced levels, so the reconstruction error for in-window values is at most half of one
    * step.
    */
  final class UniformQuantizer private (
      val lower: Double,
      val upper: Double,
      val bits: PackedBits
  ) extends PackedEncoding[Double]:
    val levels: Int =
      1 << bits.bits

    val step: Double =
      (upper - lower) / (levels - 1).toDouble

    def encode(value: Double): Int =
      if value.isNaN then 0
      else
        val clamped = math.max(lower, math.min(upper, value))
        math.round((clamped - lower) / step).toInt

    def decode(code: Int): Double =
      lower + code.toDouble * step

    def decodeToFloat(code: Int): Float =
      decode(code).toFloat

  object UniformQuantizer:
    def create(
        lower: Double,
        upper: Double,
        bits: PackedBits
    ): Either[PackedImageError, UniformQuantizer] =
      if !lower.isFinite || !upper.isFinite || lower >= upper then
        Left(
          PackedImageError.InvalidQuantizer(
            s"quantizer window must be finite with lower < upper, got [$lower, $upper]"
          )
        )
      else Right(new UniformQuantizer(lower, upper, bits))

/** Sub-byte packed companion to a dense [[Sampled]] image.
  *
  * This wrapper deliberately lives beside `Sampled`; ordinary images never grow a storage type
  * parameter. A packed image keeps the source sample space, the encoding, and canonical packed
  * codes in row-major logical order.
  */
final class PackedSampled[S <: SampleSpace[?, ?], A] private (
    val sampleSpace: S,
    val encoding: PackedEncoding[A],
    val codes: PackedArray
):
  def logicalShape: Vector[Int] =
    codes.shape

  def size: Int =
    codes.size

  def codeVector: Vector[Int] =
    codes.codeVector

  def decodeAt(linear: Int): A =
    encoding.decode(codes.codeAt(linear))

  def decodeVector: Vector[A] =
    codes.codeVector.map(encoding.decode)

  /** Fused packed-to-Float decode allocating only the output array. */
  def decodeToFloatArray: Array[Float] =
    val output = new Array[Float](size)
    var linear = 0
    while linear < size do
      output(linear) = encoding.decodeToFloat(codes.codeAt(linear))
      linear += 1
    output

object PackedSampled:
  /** Pack a dense image's samples with an explicit encoding. */
  def pack[
      S <: SampleSpace[?, ?],
      A,
      Sem,
      R <: AnyRank
  ](
      image: Sampled[S, A, Sem, R],
      encoding: PackedEncoding[A]
  ): Either[PackedImageError, PackedSampled[S, A]] =
    PackedArray
      .fromCodes(
        image.logicalShape,
        encoding.bits,
        image.data.elementsIterator.map(encoding.encode)
      )
      .map(codes => new PackedSampled(image.sampleSpace, encoding, codes))
      .left
      .map(PackedImageError.Packed.apply)

  /** Pack a Boolean mask into one-bit codes. */
  def packMask[
      S <: SampleSpace[?, ?],
      R <: AnyRank
  ](
      mask: MaskImage[S, R]
  ): Either[PackedImageError, PackedSampled[S, Boolean]] =
    pack(mask, PackedEncoding.Mask)

  private[image4s] def fromCodes[S <: SampleSpace[?, ?], A](
      sampleSpace: S,
      encoding: PackedEncoding[A],
      codes: PackedArray
  ): PackedSampled[S, A] =
    new PackedSampled(sampleSpace, encoding, codes)

/** Wordwise set algebra over packed one-bit masks.
  *
  * Operands must sample the same live grid and shape. Codes are combined thirty-two samples per
  * word without any Boolean expansion.
  */
extension [S <: SampleSpace[?, ?]](left: PackedSampled[S, Boolean])
  def union(
      right: PackedSampled[?, Boolean]
  ): Either[PackedImageError, PackedSampled[S, Boolean]] =
    combineMasks(left, right)(PackedBitOps.union)

  def intersection(
      right: PackedSampled[?, Boolean]
  ): Either[PackedImageError, PackedSampled[S, Boolean]] =
    combineMasks(left, right)(PackedBitOps.intersection)

  def difference(
      right: PackedSampled[?, Boolean]
  ): Either[PackedImageError, PackedSampled[S, Boolean]] =
    combineMasks(left, right)(PackedBitOps.difference)

  def symmetricDifference(
      right: PackedSampled[?, Boolean]
  ): Either[PackedImageError, PackedSampled[S, Boolean]] =
    combineMasks(left, right)(PackedBitOps.symmetricDifference)

  def complement: Either[PackedImageError, PackedSampled[S, Boolean]] =
    PackedBitOps
      .complement(left.codes)
      .map(codes => PackedSampled.fromCodes(left.sampleSpace, left.encoding, codes))
      .left
      .map(PackedImageError.Packed.apply)

  def countTrue: Either[PackedImageError, Long] =
    PackedBitOps
      .countTrue(left.codes)
      .left
      .map(PackedImageError.Packed.apply)

private def combineMasks[S <: SampleSpace[?, ?]](
    left: PackedSampled[S, Boolean],
    right: PackedSampled[?, Boolean]
)(
    op: (PackedArray, PackedArray) => Either[PackedError, PackedArray]
): Either[PackedImageError, PackedSampled[S, Boolean]] =
  if left.logicalShape != right.logicalShape ||
    !left.sampleSpace.grid.sameRuntimeOwnerAs(right.sampleSpace.grid)
  then
    Left(
      PackedImageError.SpaceMismatch(left.logicalShape, right.logicalShape)
    )
  else
    op(left.codes, right.codes)
      .map(codes => PackedSampled.fromCodes(left.sampleSpace, left.encoding, codes))
      .left
      .map(PackedImageError.Packed.apply)
