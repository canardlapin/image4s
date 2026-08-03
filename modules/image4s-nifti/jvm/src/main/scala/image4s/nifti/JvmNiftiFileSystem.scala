package image4s.nifti

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.InputStream
import java.io.OutputStream
import java.nio.file.Files
import java.nio.file.Path
import java.util.zip.GZIPInputStream
import java.util.zip.GZIPOutputStream
import scala.util.control.NonFatal

private object JvmNiftiFileSystem extends NiftiFileSystem[Path]:
  def show(path: Path): String =
    path.toString

  def fileName(path: Path): String =
    Option(path.getFileName).fold("")(_.toString)

  def sibling(path: Path, fileName: String): Path =
    path.resolveSibling(fileName)

  def exists(path: Path): Boolean =
    Files.exists(path)

  override def ioStrategy(_path: Path): NiftiIoStrategy =
    NiftiIoStrategy.BoundedStreaming

  def readBytes(
      path: Path,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    protect(path, operation) {
      val base =
        new BufferedInputStream(Files.newInputStream(path))
      val input: InputStream =
        if isCompressed(path) then new GZIPInputStream(base)
        else base
      try Right(input.readAllBytes())
      finally input.close()
    }

  override def readUpTo(
      path: Path,
      operation: NiftiOperation,
      maximumBytes: Int
  ): Either[NiftiError, NiftiBoundedRead] =
    protect(path, operation) {
      val input = openInput(path)
      try
        val bytes = new Array[Byte](maximumBytes)
        var offset = 0
        var ended = false
        while offset < maximumBytes && !ended do
          val read = input.read(bytes, offset, maximumBytes - offset)
          if read < 0 then ended = true
          else if read > 0 then offset += read
        if offset == maximumBytes && !ended then ended = input.read() < 0
        Right(
          NiftiBoundedRead(
            java.util.Arrays.copyOf(bytes, offset),
            ended
          )
        )
      finally input.close()
    }

  override def readChunks(
      path: Path,
      operation: NiftiOperation,
      startOffset: Long,
      byteCount: Long,
      workingBufferBytes: Int
  )(
      consume: (Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    protect(path, operation) {
      val input = openInput(path)
      try
        var skipped = 0L
        var ended = false
        while skipped < startOffset && !ended do
          val advanced = input.skip(startOffset - skipped)
          if advanced > 0L then skipped += advanced
          else
            val value = input.read()
            if value < 0 then ended = true
            else skipped += 1L
        if ended then
          Left(
            NiftiError.UnexpectedEndOfFile(
              operation,
              clampToInt(startOffset + byteCount),
              clampToInt(skipped)
            )
          )
        else
          val buffer =
            new Array[Byte](
              math.min(workingBufferBytes.toLong, byteCount).toInt
            )
          var consumed = 0L
          var failure: Option[NiftiError] = None
          while consumed < byteCount && failure.isEmpty do
            val requested =
              math.min(buffer.length.toLong, byteCount - consumed).toInt
            var filled = 0
            while filled < requested && failure.isEmpty do
              val read = input.read(buffer, filled, requested - filled)
              if read < 0 then
                failure = Some(
                  NiftiError.UnexpectedEndOfFile(
                    operation,
                    clampToInt(startOffset + byteCount),
                    clampToInt(startOffset + consumed + filled)
                  )
                )
              else if read > 0 then filled += read
            if failure.isEmpty then
              consume(buffer, requested) match
                case Left(error) => failure = Some(error)
                case Right(_) => consumed += requested.toLong
          failure.toLeft(())
      finally input.close()
    }

  def writeBytes(
      path: Path,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    protect(path, NiftiOperation.Write) {
      Option(path.getParent).foreach(Files.createDirectories(_))
      val base =
        new BufferedOutputStream(Files.newOutputStream(path))
      val output: OutputStream =
        if isCompressed(path) then new GZIPOutputStream(base)
        else base
      try
        output.write(bytes)
        Right(())
      finally output.close()
    }

  override def writeChunks(
      path: Path,
      prefix: Array[Byte],
      payloadBytes: Long,
      workingBufferBytes: Int
  )(
      fill: (Long, Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    protect(path, NiftiOperation.Write) {
      Option(path.getParent).foreach(Files.createDirectories(_))
      val output = openOutput(path)
      try
        output.write(prefix)
        val buffer =
          new Array[Byte](
            math.min(workingBufferBytes.toLong, payloadBytes).toInt
          )
        var payloadOffset = 0L
        var failure: Option[NiftiError] = None
        while payloadOffset < payloadBytes && failure.isEmpty do
          val length =
            math
              .min(
                buffer.length.toLong,
                payloadBytes - payloadOffset
              )
              .toInt
          fill(payloadOffset, buffer, length) match
            case Left(error) => failure = Some(error)
            case Right(_) =>
              output.write(buffer, 0, length)
              payloadOffset += length.toLong
        failure.toLeft(())
      finally output.close()
    }

  private def openInput(path: Path): InputStream =
    val base =
      new BufferedInputStream(Files.newInputStream(path))
    if isCompressed(path) then new GZIPInputStream(base)
    else base

  private def openOutput(path: Path): OutputStream =
    val base =
      new BufferedOutputStream(Files.newOutputStream(path))
    if isCompressed(path) then new GZIPOutputStream(base)
    else base

  private def clampToInt(value: Long): Int =
    math.max(0L, math.min(value, Int.MaxValue.toLong)).toInt

  private def isCompressed(path: Path): Boolean =
    show(path).toLowerCase.endsWith(".gz")

  private def protect[A](
      path: Path,
      operation: NiftiOperation
  )(
      body: => Either[NiftiError, A]
  ): Either[NiftiError, A] =
    try body
    catch
      case NonFatal(error) =>
        Left(
          NiftiError.IoFailure(
            show(path),
            operation,
            Option(error.getMessage).getOrElse(error.getClass.getName)
          )
        )
