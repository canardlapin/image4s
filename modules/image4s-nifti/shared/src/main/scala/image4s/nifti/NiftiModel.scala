package image4s.nifti

import image4s.ImageError
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.GeometryError
import image4s.geometry.LengthUnit

enum NiftiByteOrder derives CanEqual:
  case LittleEndian, BigEndian

enum NiftiSpatialUnit derives CanEqual:
  case Unknown, Meter, Millimeter, Micrometer

enum NiftiTemporalUnit derives CanEqual:
  case Unknown, Second, Millisecond, Microsecond, Hertz, Ppm, RadianPerSecond

enum NiftiStorage derives CanEqual:
  case SingleFile, PairFile

enum NiftiDatatype(
    val code: Int,
    val bitsPerValue: Int
) derives CanEqual:
  case UInt8 extends NiftiDatatype(2, 8)
  case Int16 extends NiftiDatatype(4, 16)
  case Int32 extends NiftiDatatype(8, 32)
  case Float32 extends NiftiDatatype(16, 32)
  case Float64 extends NiftiDatatype(64, 64)

object NiftiDatatype:
  private[nifti] def from(
      code: Int,
      bitsPerValue: Int
  ): Either[NiftiError, NiftiDatatype] =
    NiftiDatatype.values.find(_.code == code) match
      case Some(datatype) if datatype.bitsPerValue == bitsPerValue =>
        Right(datatype)
      case _ =>
        Left(NiftiError.UnsupportedDatatype(code, bitsPerValue))

enum NiftiIntegerConversion derives CanEqual:
  case RejectLossy, RoundToNearestEven

sealed trait NiftiWriteOptionsError derives CanEqual:
  def message: String

object NiftiWriteOptionsError:
  final case class InvalidSlope(value: Double)
      extends NiftiWriteOptionsError:
    val message: String =
      s"NIfTI write slope must remain finite and nonzero in its Float32 header field, got $value"

  final case class InvalidIntercept(value: Double)
      extends NiftiWriteOptionsError:
    val message: String =
      s"NIfTI write intercept must remain finite in its Float32 header field, got $value"

  final case class InvalidNonSpatialPixelDimension(
      axis: Int,
      value: Double
  ) extends NiftiWriteOptionsError:
    val message: String =
      s"NIfTI write pixel dimension ${axis + 4} must remain finite and positive in its Float32 header field, got $value"

final class NiftiWriteOptions private (
    val datatype: NiftiDatatype,
    val slope: Double,
    val intercept: Double,
    val integerConversion: NiftiIntegerConversion,
    val nonSpatialPixelDimensions: Vector[Double],
    val temporalUnit: NiftiTemporalUnit
) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: NiftiWriteOptions =>
        datatype == that.datatype &&
        slope == that.slope &&
        intercept == that.intercept &&
        integerConversion == that.integerConversion &&
        nonSpatialPixelDimensions == that.nonSpatialPixelDimensions &&
        temporalUnit == that.temporalUnit
      case _ =>
        false

  override def hashCode(): Int =
    Seq(
      datatype,
      slope,
      intercept,
      integerConversion,
      nonSpatialPixelDimensions,
      temporalUnit
    ).hashCode

  override def toString: String =
    s"NiftiWriteOptions($datatype,$slope,$intercept,$integerConversion,$nonSpatialPixelDimensions,$temporalUnit)"

  def withNonSpatialSampling(
      pixelDimensions: Vector[Double],
      unit: NiftiTemporalUnit
  ): Either[NiftiWriteOptionsError, NiftiWriteOptions] =
    NiftiWriteOptions
      .validateNonSpatialPixelDimensions(pixelDimensions)
      .map { stored =>
        new NiftiWriteOptions(
          datatype,
          slope,
          intercept,
          integerConversion,
          stored,
          unit
        )
      }

object NiftiWriteOptions:
  val default: NiftiWriteOptions =
    forDatatype(NiftiDatatype.Float64)

  def forDatatype(datatype: NiftiDatatype): NiftiWriteOptions =
    new NiftiWriteOptions(
      datatype,
      slope = 1.0,
      intercept = 0.0,
      integerConversion = NiftiIntegerConversion.RejectLossy,
      nonSpatialPixelDimensions = Vector.empty,
      temporalUnit = NiftiTemporalUnit.Unknown
    )

  def create(
      datatype: NiftiDatatype,
      slope: Double,
      intercept: Double,
      integerConversion: NiftiIntegerConversion =
        NiftiIntegerConversion.RejectLossy,
      nonSpatialPixelDimensions: Vector[Double] = Vector.empty,
      temporalUnit: NiftiTemporalUnit = NiftiTemporalUnit.Unknown
  ): Either[NiftiWriteOptionsError, NiftiWriteOptions] =
    val storedSlope = slope.toFloat
    val storedIntercept = intercept.toFloat
    if !storedSlope.isFinite || storedSlope == 0.0f then
      Left(NiftiWriteOptionsError.InvalidSlope(slope))
    else if !storedIntercept.isFinite then
      Left(NiftiWriteOptionsError.InvalidIntercept(intercept))
    else
      validateNonSpatialPixelDimensions(
        nonSpatialPixelDimensions
      ).map { stored =>
        new NiftiWriteOptions(
          datatype,
          storedSlope.toDouble,
          storedIntercept.toDouble,
          integerConversion,
          stored,
          temporalUnit
        )
      }

  private def validateNonSpatialPixelDimensions(
      values: Vector[Double]
  ): Either[NiftiWriteOptionsError, Vector[Double]] =
    val stored = values.map(_.toFloat)
    stored.zip(values).zipWithIndex.collectFirst {
      case ((rounded, original), axis)
          if !rounded.isFinite || rounded <= 0.0f =>
        NiftiWriteOptionsError.InvalidNonSpatialPixelDimension(
          axis,
          original
        )
    }.toLeft(stored.map(_.toDouble))

sealed trait NiftiExtensionError derives CanEqual:
  def message: String

object NiftiExtensionError:
  final case class InvalidCode(code: Int) extends NiftiExtensionError:
    val message: String =
      s"NIfTI extension code must be non-negative, got $code"

  final case class ContentTooLarge(bytes: Long) extends NiftiExtensionError:
    val message: String =
      s"NIfTI extension content requires $bytes bytes, beyond the supported limit"

  final case class InvalidBlockSize(
      index: Int,
      size: Int
  ) extends NiftiExtensionError:
    val message: String =
      s"NIfTI extension $index has size $size; block sizes must be at least 16"

  final case class MisalignedBlockSize(
      index: Int,
      size: Int
  ) extends NiftiExtensionError:
    val message: String =
      s"NIfTI extension $index has size $size; block sizes must be multiples of 16"

  final case class BlockExceedsRegion(
      index: Int,
      size: Int,
      remainingBytes: Int
  ) extends NiftiExtensionError:
    val message: String =
      s"NIfTI extension $index declares $size bytes with only $remainingBytes remaining"

final class NiftiExtension private (
    val code: Int,
    val payload: Vector[Byte]
) derives CanEqual:
  def encodedSize: Int =
    payload.length + 8

  override def equals(other: Any): Boolean =
    other match
      case that: NiftiExtension =>
        code == that.code && payload == that.payload
      case _ =>
        false

  override def hashCode(): Int =
    31 * code.hashCode + payload.hashCode

  override def toString: String =
    s"NiftiExtension(code=$code, encodedSize=$encodedSize)"

object NiftiExtension:
  def create(
      code: Int,
      content: IterableOnce[Byte]
  ): Either[NiftiExtensionError, NiftiExtension] =
    if code < 0 then
      Left(NiftiExtensionError.InvalidCode(code))
    else
      val bytes = content.iterator.toVector
      val unpaddedSize = bytes.length.toLong + 8L
      val encodedSize = ((unpaddedSize + 15L) / 16L) * 16L
      if encodedSize > Int.MaxValue.toLong then
        Left(NiftiExtensionError.ContentTooLarge(bytes.length.toLong))
      else
        val padding = encodedSize.toInt - 8 - bytes.length
        Right(
          new NiftiExtension(
            code,
            bytes ++ Vector.fill(padding)(0.toByte)
          )
        )

  private[nifti] def decoded(
      code: Int,
      payload: Vector[Byte]
  ): Either[NiftiExtensionError, NiftiExtension] =
    if code < 0 then
      Left(NiftiExtensionError.InvalidCode(code))
    else
      Right(new NiftiExtension(code, payload))

final case class NiftiHeader(
    dimensions: Vector[Int],
    pixelDimensions: Vector[Double],
    datatype: NiftiDatatype,
    voxelOffset: Int,
    slope: Double,
    intercept: Double,
    qformCode: Int,
    qform: Option[Affine[D3]],
    sformCode: Int,
    sform: Option[Affine[D3]],
    fallbackAffine: Affine[D3],
    byteOrder: NiftiByteOrder,
    spatialUnit: NiftiSpatialUnit,
    temporalUnit: NiftiTemporalUnit,
    storage: NiftiStorage,
    extensions: Vector[NiftiExtension]
):
  def preferredAffine: Affine[D3] =
    sform.orElse(qform).getOrElse(fallbackAffine)

  def effectiveSlope: Double =
    if slope == 0.0 then 1.0 else slope

  def spatialShape: Vector[Int] =
    dimensions.padTo(3, 1).take(3)

  def nonSpatialShape: Vector[Int] =
    dimensions.drop(3)

  def logicalShape: Vector[Int] =
    spatialShape ++ nonSpatialShape

final case class NiftiReadOptions(
    fallbackSpatialUnit: LengthUnit
)

object NiftiReadOptions:
  val default: NiftiReadOptions =
    NiftiReadOptions(LengthUnit.Millimeter)

enum NiftiOperation derives CanEqual:
  case ReadHeader, ReadPayload, Write

enum NiftiHeaderField derives CanEqual:
  case HeaderSize
  case Magic
  case DimensionCount
  case Dimension(axis: Int)
  case PixelDimension(axis: Int)
  case VoxelOffset
  case Scaling
  case ExtensionFlag

enum NiftiValueProblem derives CanEqual:
  case NonFinite
  case Fractional
  case OutsideRange(
      minimum: Double,
      maximum: Double
  )
  case FloatingOverflow

enum NiftiFiles[+P] derives CanEqual:
  case SingleFile(path: P)
  case PairFile(
      headerPath: P,
      imagePath: P
  )

  def paths: Vector[P] =
    this match
      case NiftiFiles.SingleFile(path) =>
        Vector(path)
      case NiftiFiles.PairFile(headerPath, imagePath) =>
        Vector(headerPath, imagePath)

sealed trait NiftiError derives CanEqual:
  def message: String

object NiftiError:
  final case class IoFailure(
      path: String,
      operation: NiftiOperation,
      detail: String
  ) extends NiftiError:
    val message: String =
      s"$operation failed for $path: $detail"

  final case class InvalidHeader(
      field: NiftiHeaderField,
      detail: String
  ) extends NiftiError:
    val message: String =
      s"invalid NIfTI-1 $field: $detail"

  final case class UnsupportedDatatype(
      code: Int,
      bitsPerValue: Int
  ) extends NiftiError:
    val message: String =
      s"unsupported NIfTI datatype $code with $bitsPerValue bits per value"

  final case class UnexpectedEndOfFile(
      operation: NiftiOperation,
      expectedBytes: Int,
      actualBytes: Int
  ) extends NiftiError:
    val message: String =
      s"unexpected end of NIfTI input during $operation: " +
        s"expected $expectedBytes bytes, got $actualBytes"

  final case class InvalidArrayShape(detail: String) extends NiftiError:
    val message: String =
      s"NIfTI dimensions cannot form a Ravel array: $detail"

  final case class Geometry(error: GeometryError) extends NiftiError:
    val message: String =
      error.message

  final case class Image(error: ImageError) extends NiftiError:
    val message: String =
      error.message

  final case class FrameConventionMismatch(
      actual: CoordinateConvention
  ) extends NiftiError:
    val message: String =
      s"NIfTI frames use RAS coordinates, but the supplied frame declares $actual"

  final case class FrameUnitMismatch(
      fileUnit: NiftiSpatialUnit,
      frameUnit: LengthUnit
  ) extends NiftiError:
    val message: String =
      s"NIfTI spatial unit $fileUnit does not match supplied frame unit $frameUnit"

  final case class UnsupportedWriteRank(rank: Int) extends NiftiError:
    val message: String =
      s"NIfTI-1 output supports three through seven logical axes, got $rank"

  final case class NonSpatialPixelDimensionCount(
      supplied: Int,
      available: Int
  ) extends NiftiError:
    val message: String =
      s"NIfTI write options supplied $supplied non-spatial pixel dimensions for $available non-spatial axes"

  final case class OutputTooLarge(bytes: Long) extends NiftiError:
    val message: String =
      s"NIfTI output requires $bytes bytes, beyond the supported in-memory writer limit"

  final case class ValueNotRepresentable(
      logicalIndex: Vector[Int],
      value: Double,
      encodedValue: Double,
      datatype: NiftiDatatype,
      problem: NiftiValueProblem
  ) extends NiftiError:
    val message: String =
      s"NIfTI value $value at ${logicalIndex.mkString("[", ",", "]")} " +
        s"cannot be encoded as $datatype (raw=$encodedValue): $problem"

  final case class UnsupportedPath(path: String) extends NiftiError:
    val message: String =
      s"unsupported NIfTI path $path; expected .nii, .nii.gz, .hdr, .img, .hdr.gz, or .img.gz"

  final case class MissingCompanion(
      entryPath: String,
      companionPath: String
  ) extends NiftiError:
    val message: String =
      s"NIfTI pair entered through $entryPath is missing companion $companionPath"

  final case class StorageMismatch(
      path: String,
      expected: NiftiStorage,
      actual: NiftiStorage
  ) extends NiftiError:
    val message: String =
      s"NIfTI path $path implies $expected storage but its magic declares $actual"

  final case class Extension(error: NiftiExtensionError)
      extends NiftiError:
    val message: String =
      error.message

final case class DecodedNifti[I](
    image: I,
    header: NiftiHeader
)
