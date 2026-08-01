package image4s.nifti

/**
 * Platform boundary for basic synchronous NIfTI filesystem operations.
 *
 * Format parsing, validation, byte order, storage variants, and numeric
 * conversion remain in shared code. JVM and Node.js implementations own only
 * path handling, physical file access, and gzip. The default chunk methods are
 * whole-file compatibility implementations for small in-memory test adapters;
 * production adapters declare their strategy and override them.
 */
private[nifti] trait NiftiFileSystem[P]:
  def show(path: P): String

  def fileName(path: P): String

  def sibling(path: P, fileName: String): P

  def exists(path: P): Boolean

  def ioStrategy(path: P): NiftiIoStrategy

  def readBytes(
      path: P,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]]

  /** Read at most `maximumBytes`, reporting whether the logical stream ended. */
  def readUpTo(
      path: P,
      operation: NiftiOperation,
      maximumBytes: Int
  ): Either[NiftiError, NiftiBoundedRead] =
    readBytes(path, operation).map { all =>
      val length = math.min(all.length, maximumBytes)
      NiftiBoundedRead(
        java.util.Arrays.copyOf(all, length),
        all.length <= maximumBytes
      )
    }

  /** Consume an exact logical byte range through one reusable working buffer. */
  def readChunks(
      path: P,
      operation: NiftiOperation,
      startOffset: Long,
      byteCount: Long,
      workingBufferBytes: Int
  )(
      consume: (Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    readBytes(path, operation).flatMap { all =>
      val expectedEnd = startOffset + byteCount
      if startOffset < 0L ||
        byteCount < 0L ||
        expectedEnd > all.length.toLong
      then
        Left(
          NiftiError.UnexpectedEndOfFile(
            operation,
            NiftiFileSystem.clampToInt(expectedEnd),
            all.length
          )
        )
      else
        val buffer =
          new Array[Byte](math.min(workingBufferBytes.toLong, byteCount).toInt)
        var sourceOffset = startOffset.toInt
        var remaining = byteCount
        var failure: Option[NiftiError] = None
        while remaining > 0L && failure.isEmpty do
          val length = math.min(buffer.length.toLong, remaining).toInt
          System.arraycopy(all, sourceOffset, buffer, 0, length)
          consume(buffer, length) match
            case Left(error) => failure = Some(error)
            case Right(_) =>
              sourceOffset += length
              remaining -= length.toLong
        failure.toLeft(())
    }

  def writeBytes(
      path: P,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit]

  /** Write a bounded prefix followed by a generated payload in file order. */
  def writeChunks(
      path: P,
      prefix: Array[Byte],
      payloadBytes: Long,
      workingBufferBytes: Int
  )(
      fill: (Long, Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    val total = prefix.length.toLong + payloadBytes
    if total > Int.MaxValue.toLong then
      Left(NiftiError.OutputTooLarge(total))
    else
      val all = new Array[Byte](total.toInt)
      System.arraycopy(prefix, 0, all, 0, prefix.length)
      val buffer =
        new Array[Byte](
          math.min(workingBufferBytes.toLong, payloadBytes).toInt
        )
      var payloadOffset = 0L
      var failure: Option[NiftiError] = None
      while payloadOffset < payloadBytes && failure.isEmpty do
        val length =
          math.min(buffer.length.toLong, payloadBytes - payloadOffset).toInt
        fill(payloadOffset, buffer, length) match
          case Left(error) => failure = Some(error)
          case Right(_) =>
            System.arraycopy(
              buffer,
              0,
              all,
              prefix.length + payloadOffset.toInt,
              length
            )
            payloadOffset += length.toLong
      failure match
        case Some(error) => Left(error)
        case None        => writeBytes(path, all)

private[nifti] final case class NiftiBoundedRead(
    bytes: Array[Byte],
    endOfInput: Boolean
)

private object NiftiFileSystem:
  def clampToInt(value: Long): Int =
    math.max(0L, math.min(value, Int.MaxValue.toLong)).toInt
