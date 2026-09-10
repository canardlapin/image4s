package image4s.nifti

import image4s.NonSpatialAxes
import image4s.geometry.{D3, Frame, Grid}
import java.nio.{ByteBuffer, ByteOrder}

/** One exclusive, synchronous staging-file writer, bound to its supplied geometry and axes.
  *
  * Spatial indices are NIfTI first-axis-fastest: x + nx * (y + ny * z). Non-spatial coordinates
  * follow axes.values, with the first non-spatial axis fastest in the file. These are not Ravel
  * linear offsets. Calls may arrive in any order; repeated samples are overwritten in call order.
  * Unwritten samples retain stored code zero (scaled value options.intercept).
  *
  * Invalid blocks are refused before any of their bytes are written and may be corrected. Physical
  * IO failure closes and poisons the writer. close is idempotent, reports prior IO failure, and
  * releases resources; it does not certify coverage or publish a scientific result. The caller owns
  * staging-file removal/publication. Use Nifti.withScalarWriter for exception-safe resource scope.
  * Do not call concurrently or mutate supplied arrays during a call. Input arrays are not retained.
  */
final class NiftiScalarWriter[F <: Frame[D3], P] private[nifti] (
    val path: P,
    val grid: Grid[F, D3],
    val axes: NonSpatialAxes,
    val options: NiftiWriteOptions,
    val payloadOffset: Long,
    val payloadBytes: Long,
    output: NiftiSeekableOutput,
    encoder: NiftiScalarEncoder
):
  val spatialSize: Long = grid.shape.foldLeft(1L)(_ * _.toLong)
  private val bytesPerValue = options.datatype.bitsPerValue / 8
  private val bufferBytes =
    math.max(bytesPerValue, options.ioLimits.workingBufferBytes / bytesPerValue * bytesPerValue)
  private val bytes = new Array[Byte](math.min(bufferBytes.toLong, payloadBytes).toInt)
  private val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
  private val index = new Array[Int](3 + axes.size)
  private var closed = false
  private var failure: Option[NiftiError] = None

  /** Write a contiguous spatial span at one non-spatial coordinate, without an index array. */
  def writeSpatialSpan(
      nonSpatialIndex: Vector[Int],
      firstSpatialIndex: Long,
      values: Array[Double]
  ): Either[NiftiError, Unit] =
    if firstSpatialIndex < 0L || firstSpatialIndex > spatialSize ||
      values.length.toLong > spatialSize - firstSpatialIndex
    then
      Left(
        NiftiError.InvalidOutputBlock(
          s"span $firstSpatialIndex + ${values.length} exceeds $spatialSize spatial samples"
        )
      )
    else writeBlock(nonSpatialIndex, values, i => firstSpatialIndex + i.toLong)

  /** Write sparse samples; adjacent indices are coalesced into bounded physical writes. */
  def writeSpatialBlock(
      nonSpatialIndex: Vector[Int],
      spatialIndices: Array[Long],
      values: Array[Double]
  ): Either[NiftiError, Unit] =
    if spatialIndices.length != values.length then
      Left(
        NiftiError.InvalidOutputBlock(
          s"${spatialIndices.length} indices for ${values.length} values"
        )
      )
    else writeBlock(nonSpatialIndex, values, i => spatialIndices(i))

  def close(): Either[NiftiError, Unit] =
    if !closed then
      closed = true
      output.close() match
        case Left(closing) =>
          failure = Some(
            failure.fold(closing)(primary => NiftiError.OutputCloseFailure(primary, closing))
          )
        case Right(_) => ()
    failure.toLeft(())

  private def writeBlock(
      nonSpatialIndex: Vector[Int],
      values: Array[Double],
      spatialAt: Int => Long
  ): Either[NiftiError, Unit] =
    if failure.nonEmpty then failure.toLeft(())
    else if closed then Left(NiftiError.OutputClosed)
    else if nonSpatialIndex.size != axes.size then
      Left(
        NiftiError.InvalidOutputBlock(
          s"${nonSpatialIndex.size} non-spatial indices for ${axes.size} axes"
        )
      )
    else
      var frameOffset = 0L
      var frameStride = spatialSize
      var axis = 0
      var invalid: Option[NiftiError] = None
      while axis < axes.size && invalid.isEmpty do
        val coordinate = nonSpatialIndex(axis)
        if coordinate < 0 || coordinate >= axes.shape(axis) then
          invalid = Some(
            NiftiError.InvalidOutputBlock(
              s"non-spatial index $coordinate outside axis $axis extent ${axes.shape(axis)}"
            )
          )
        else
          frameOffset += coordinate.toLong * frameStride
          frameStride *= axes.shape(axis).toLong
          index(axis + 3) = coordinate
        axis += 1
      // Validate all coordinates and conversions before writing any byte from this block.
      var valueIndex = 0
      try
        while valueIndex < values.length && invalid.isEmpty do
          val spatial = spatialAt(valueIndex)
          if spatial < 0L || spatial >= spatialSize then
            invalid = Some(
              NiftiError.InvalidOutputBlock(s"spatial index $spatial outside [0,$spatialSize)")
            )
          else
            decodeSpatial(spatial)
            encoder.encode(buffer, 0, index, values(valueIndex))
          valueIndex += 1
      catch case conversion: NiftiWriteConversionFailure => invalid = Some(conversion.error)
      invalid match
        case Some(error) => Left(error)
        case None =>
          valueIndex = 0
          while valueIndex < values.length && failure.isEmpty do
            val firstSpatial = spatialAt(valueIndex)
            var count = 0
            while valueIndex + count < values.length && count < bytes.length / bytesPerValue &&
              spatialAt(valueIndex + count) == firstSpatial + count.toLong
            do
              decodeSpatial(firstSpatial + count.toLong)
              encoder.encode(buffer, count * bytesPerValue, index, values(valueIndex + count))
              count += 1
            output.writeAt(
              payloadOffset + (frameOffset + firstSpatial) * bytesPerValue.toLong,
              bytes,
              count * bytesPerValue
            ) match
              case Left(error) =>
                failure = Some(error)
                val _ = close()
              case Right(_) => valueIndex += count
          failure.toLeft(())

  private def decodeSpatial(spatial: Long): Unit =
    var remaining = spatial
    var axis = 0
    while axis < 3 do
      index(axis) = (remaining % grid.shape(axis).toLong).toInt
      remaining /= grid.shape(axis).toLong
      axis += 1

private[nifti] trait NiftiScalarEncoder:
  def encode(buffer: ByteBuffer, offset: Int, index: Array[Int], value: Double): Unit

private[nifti] final case class NiftiWriteConversionFailure(error: NiftiError)
    extends RuntimeException(error.message)
