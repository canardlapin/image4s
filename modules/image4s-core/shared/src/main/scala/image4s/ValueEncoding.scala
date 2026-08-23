package image4s

import ravel.{UInt8 as RavelUInt8}

/** Structural, serializable relationship between stored and domain values.
  *
  * Encodings carry data only: they never accept a user callback. Their fingerprints are stable
  * across processes and are suitable for persistent receipts and equality checks.
  */
sealed trait ValueEncoding[Stored, Domain] derives CanEqual:
  def fingerprint: String

  /** Decode one stored value.
    *
    * `nonSpatialIndex` is required only by encodings whose coefficients vary along a declared
    * non-spatial axis.
    */
  def decode(
      stored: Stored,
      nonSpatialIndex: Vector[Int]
  ): Either[EncodingError, Domain]

  private[image4s] def validateFor(
      nonSpatialShape: Vector[Int]
  ): Either[EncodingError, Unit]

object ValueEncoding:
  final case class Identity[A]() extends ValueEncoding[A, A]:
    val fingerprint: String =
      "image4s:value-encoding:identity:v1"

    def decode(
        stored: A,
        nonSpatialIndex: Vector[Int]
    ): Either[EncodingError, A] =
      Right(stored)

    private[image4s] def validateFor(
        nonSpatialShape: Vector[Int]
    ): Either[EncodingError, Unit] =
      Right(())

  final case class UniformAffine private (
      slope: Double,
      intercept: Double
  ) extends ValueEncoding[Double, Double]:
    val fingerprint: String =
      s"image4s:value-encoding:uniform-affine:v1:${doubleToken(slope)}:${doubleToken(intercept)}"

    def decode(
        stored: Double,
        nonSpatialIndex: Vector[Int]
    ): Either[EncodingError, Double] =
      Right(stored * slope + intercept)

    private[image4s] def validateFor(
        nonSpatialShape: Vector[Int]
    ): Either[EncodingError, Unit] =
      Right(())

  object UniformAffine:
    def create(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, UniformAffine] =
      if !slope.isFinite || slope == 0.0 then
        Left(EncodingError.InvalidParameter("UniformAffine.slope", slope.toString))
      else if !intercept.isFinite then
        Left(
          EncodingError.InvalidParameter(
            "UniformAffine.intercept",
            intercept.toString
          )
        )
      else Right(new UniformAffine(slope, intercept))

  /** A structural affine decode from one primitive storage dtype to the `Double` domain.
    *
    * The storage witness is part of the concrete encoding and its stable fingerprint. This lets
    * encoded image formats retain their native Ravel dtype without hiding a widening copy or a
    * runtime callback.
    */
  sealed trait PrimitiveToDoubleAffine[Stored] extends ValueEncoding[Stored, Double]:
    def slope: Double
    def intercept: Double

    protected def storageId: String
    protected def widen(stored: Stored): Double

    final def fingerprint: String =
      s"image4s:value-encoding:primitive-to-double-affine:v1:$storageId:" +
        s"${doubleToken(slope)}:${doubleToken(intercept)}"

    final def decode(
        stored: Stored,
        nonSpatialIndex: Vector[Int]
    ): Either[EncodingError, Double] =
      Right(widen(stored) * slope + intercept)

    private[image4s] final def validateFor(
        nonSpatialShape: Vector[Int]
    ): Either[EncodingError, Unit] =
      Right(())

  object PrimitiveToDoubleAffine:
    def uint8(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, PrimitiveToDoubleAffine[RavelUInt8]] =
      validated(slope, intercept)(UInt8Affine.apply)

    def int16(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, PrimitiveToDoubleAffine[Short]] =
      validated(slope, intercept)(Int16Affine.apply)

    def int32(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, PrimitiveToDoubleAffine[Int]] =
      validated(slope, intercept)(Int32Affine.apply)

    def float32(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, PrimitiveToDoubleAffine[Float]] =
      validated(slope, intercept)(Float32Affine.apply)

    def float64(
        slope: Double,
        intercept: Double
    ): Either[EncodingError, PrimitiveToDoubleAffine[Double]] =
      validated(slope, intercept)(Float64Affine.apply)

    private def validated[A](
        slope: Double,
        intercept: Double
    )(
        make: (Double, Double) => PrimitiveToDoubleAffine[A]
    ): Either[EncodingError, PrimitiveToDoubleAffine[A]] =
      if !slope.isFinite || slope == 0.0 then
        Left(
          EncodingError.InvalidParameter(
            "PrimitiveToDoubleAffine.slope",
            slope.toString
          )
        )
      else if !intercept.isFinite then
        Left(
          EncodingError.InvalidParameter(
            "PrimitiveToDoubleAffine.intercept",
            intercept.toString
          )
        )
      else Right(make(slope, intercept))

    private final case class UInt8Affine(
        slope: Double,
        intercept: Double
    ) extends PrimitiveToDoubleAffine[RavelUInt8]:
      protected val storageId: String = "uint8"
      protected inline def widen(stored: RavelUInt8): Double =
        stored.toInt.toDouble

    private final case class Int16Affine(
        slope: Double,
        intercept: Double
    ) extends PrimitiveToDoubleAffine[Short]:
      protected val storageId: String = "int16"
      protected inline def widen(stored: Short): Double =
        stored.toDouble

    private final case class Int32Affine(
        slope: Double,
        intercept: Double
    ) extends PrimitiveToDoubleAffine[Int]:
      protected val storageId: String = "int32"
      protected inline def widen(stored: Int): Double =
        stored.toDouble

    private final case class Float32Affine(
        slope: Double,
        intercept: Double
    ) extends PrimitiveToDoubleAffine[Float]:
      protected val storageId: String = "float32"
      protected inline def widen(stored: Float): Double =
        stored.toDouble

    private final case class Float64Affine(
        slope: Double,
        intercept: Double
    ) extends PrimitiveToDoubleAffine[Double]:
      protected val storageId: String = "float64"
      protected inline def widen(stored: Double): Double =
        stored

  /** Affine coefficients aligned to one declared non-spatial axis. */
  final case class PerAxisAffine private (
      axis: Int,
      slopes: Vector[Double],
      intercepts: Vector[Double]
  ) extends ValueEncoding[Double, Double]:
    val fingerprint: String =
      s"image4s:value-encoding:per-axis-affine:v1:$axis:" +
        slopes.map(doubleToken).mkString(",") + ":" +
        intercepts.map(doubleToken).mkString(",")

    def decode(
        stored: Double,
        nonSpatialIndex: Vector[Int]
    ): Either[EncodingError, Double] =
      if axis < 0 || axis >= nonSpatialIndex.length then
        Left(
          EncodingError.MissingAxisCoordinate(
            axis,
            nonSpatialIndex.length
          )
        )
      else
        val coordinate = nonSpatialIndex(axis)
        if coordinate < 0 || coordinate >= slopes.length then
          Left(
            EncodingError.InvalidAxisCoordinate(
              axis,
              coordinate,
              slopes.length
            )
          )
        else Right(stored * slopes(coordinate) + intercepts(coordinate))

    private[image4s] def validateFor(
        nonSpatialShape: Vector[Int]
    ): Either[EncodingError, Unit] =
      if axis < 0 || axis >= nonSpatialShape.length then
        Left(EncodingError.MissingAxisCoordinate(axis, nonSpatialShape.length))
      else if slopes.length != nonSpatialShape(axis) then
        Left(
          EncodingError.AxisLengthMismatch(
            axis,
            nonSpatialShape(axis),
            slopes.length
          )
        )
      else Right(())

  object PerAxisAffine:
    def create(
        axis: Int,
        slopes: IterableOnce[Double],
        intercepts: IterableOnce[Double]
    ): Either[EncodingError, PerAxisAffine] =
      val copiedSlopes = slopes.iterator.toVector
      val copiedIntercepts = intercepts.iterator.toVector
      if copiedSlopes.isEmpty || copiedSlopes.length != copiedIntercepts.length then
        Left(EncodingError.InvalidParameter("PerAxisAffine", "lengths"))
      else if copiedSlopes.exists(value => !value.isFinite || value == 0.0) then
        Left(EncodingError.InvalidParameter("PerAxisAffine.slopes", "non-finite or zero"))
      else if copiedIntercepts.exists(value => !value.isFinite) then
        Left(EncodingError.InvalidParameter("PerAxisAffine.intercepts", "non-finite"))
      else Right(new PerAxisAffine(axis, copiedSlopes, copiedIntercepts))

  /** Deterministic v1 string label table indexed by Int storage codes. */
  final case class Codebook private (
      labels: Vector[String]
  ) extends ValueEncoding[Int, String]:
    val fingerprint: String =
      "image4s:value-encoding:codebook:v1:" +
        labels.map(label => s"${label.length}:$label").mkString("|")

    def decode(
        stored: Int,
        nonSpatialIndex: Vector[Int]
    ): Either[EncodingError, String] =
      labels
        .lift(stored)
        .toRight(
          EncodingError.InvalidStoredValue("Codebook", stored.toString)
        )

    private[image4s] def validateFor(
        nonSpatialShape: Vector[Int]
    ): Either[EncodingError, Unit] =
      Right(())

  object Codebook:
    def create(
        labels: IterableOnce[String]
    ): Either[EncodingError, Codebook] =
      val copied = labels.iterator.toVector
      if copied.isEmpty then Left(EncodingError.InvalidParameter("Codebook.labels", "empty"))
      else if copied.exists(label => label.isEmpty || label != label.trim) then
        Left(EncodingError.InvalidParameter("Codebook.labels", "blank or padded label"))
      else Right(new Codebook(copied))

  private def doubleToken(value: Double): String =
    java.lang.Double.toHexString(value)

sealed trait EncodingError derives CanEqual:
  def message: String

object EncodingError:
  final case class InvalidParameter(
      name: String,
      value: String
  ) extends EncodingError:
    def message: String =
      s"invalid $name: $value"

  final case class InvalidStoredValue(
      encoding: String,
      value: String
  ) extends EncodingError:
    def message: String =
      s"$value is not representable by $encoding"

  final case class MissingAxisCoordinate(
      axis: Int,
      available: Int
  ) extends EncodingError:
    def message: String =
      s"encoding requires non-spatial axis $axis, but only $available are available"

  final case class InvalidAxisCoordinate(
      axis: Int,
      coordinate: Int,
      extent: Int
  ) extends EncodingError:
    def message: String =
      s"coordinate $coordinate is outside encoding axis $axis of extent $extent"

  final case class AxisLengthMismatch(
      axis: Int,
      expected: Int,
      actual: Int
  ) extends EncodingError:
    def message: String =
      s"encoding axis $axis has $actual coefficients, expected $expected"
