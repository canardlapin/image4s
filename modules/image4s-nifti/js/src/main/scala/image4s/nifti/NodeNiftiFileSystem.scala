package image4s.nifti

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

private object NodeNiftiFileSystem extends NiftiFileSystem[String]:
  def show(path: String): String =
    path

  def fileName(path: String): String =
    NodePath.basename(path)

  def sibling(path: String, fileName: String): String =
    NodePath.join(NodePath.dirname(path), fileName)

  def exists(path: String): Boolean =
    NodeFs.existsSync(path)

  override def ioStrategy(path: String): NiftiIoStrategy =
    if isCompressed(path) then
      NiftiIoStrategy.WholeFileCompressedCompatibility
    else NiftiIoStrategy.BoundedStreaming

  def readBytes(
      path: String,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    protect(path, operation) {
      val physical = NodeFs.readFileSync(path)
      val logical =
        if isCompressed(path) then NodeZlib.gunzipSync(physical)
        else physical
      Right(toScalaBytes(logical))
    }

  override def readUpTo(
      path: String,
      operation: NiftiOperation,
      maximumBytes: Int
  ): Either[NiftiError, NiftiBoundedRead] =
    if isCompressed(path) then super.readUpTo(path, operation, maximumBytes)
    else
      protect(path, operation) {
        val descriptor = NodeFs.openSync(path, "r")
        try
          val physical = new Uint8Array(maximumBytes + 1)
          val count =
            NodeFs.readSync(
              descriptor,
              physical,
              0,
              maximumBytes + 1,
              0.0
            )
          val copied = new Array[Byte](math.min(count, maximumBytes))
          copyToScala(physical, copied, copied.length)
          Right(NiftiBoundedRead(copied, count <= maximumBytes))
        finally NodeFs.closeSync(descriptor)
      }

  override def readChunks(
      path: String,
      operation: NiftiOperation,
      startOffset: Long,
      byteCount: Long,
      workingBufferBytes: Int
  )(
      consume: (Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    if isCompressed(path) then
      super.readChunks(
        path,
        operation,
        startOffset,
        byteCount,
        workingBufferBytes
      )(consume)
    else
      protect(path, operation) {
        val descriptor = NodeFs.openSync(path, "r")
        try
          val length =
            math.min(workingBufferBytes.toLong, byteCount).toInt
          val physical = new Uint8Array(length)
          val logical = new Array[Byte](length)
          var consumed = 0L
          var failure: Option[NiftiError] = None
          while consumed < byteCount && failure.isEmpty do
            val requested =
              math.min(length.toLong, byteCount - consumed).toInt
            val read =
              NodeFs.readSync(
                descriptor,
                physical,
                0,
                requested,
                (startOffset + consumed).toDouble
              )
            if read != requested then
              failure =
                Some(
                  NiftiError.UnexpectedEndOfFile(
                    operation,
                    clampToInt(startOffset + byteCount),
                    clampToInt(startOffset + consumed + math.max(read, 0))
                  )
                )
            else
              copyToScala(physical, logical, requested)
              consume(logical, requested) match
                case Left(error) => failure = Some(error)
                case Right(_)    => consumed += requested.toLong
          failure.toLeft(())
        finally NodeFs.closeSync(descriptor)
      }

  def writeBytes(
      path: String,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    protect(path, NiftiOperation.Write) {
      val parent = NodePath.dirname(path)
      NodeFs.mkdirSync(
        parent,
        js.Dynamic.literal(recursive = true)
      )
      val logical = toUint8Array(bytes)
      val physical =
        if isCompressed(path) then NodeZlib.gzipSync(logical)
        else logical
      NodeFs.writeFileSync(path, physical)
      Right(())
    }

  override def writeChunks(
      path: String,
      prefix: Array[Byte],
      payloadBytes: Long,
      workingBufferBytes: Int
  )(
      fill: (Long, Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    if isCompressed(path) then
      super.writeChunks(
        path,
        prefix,
        payloadBytes,
        workingBufferBytes
      )(fill)
    else
      protect(path, NiftiOperation.Write) {
        val parent = NodePath.dirname(path)
        NodeFs.mkdirSync(
          parent,
          js.Dynamic.literal(recursive = true)
        )
        val descriptor = NodeFs.openSync(path, "w")
        try
          writePhysical(descriptor, toUint8Array(prefix), 0.0)
          val length =
            math.min(workingBufferBytes.toLong, payloadBytes).toInt
          val logical = new Array[Byte](length)
          var payloadOffset = 0L
          var failure: Option[NiftiError] = None
          while payloadOffset < payloadBytes && failure.isEmpty do
            val requested =
              math.min(length.toLong, payloadBytes - payloadOffset).toInt
            fill(payloadOffset, logical, requested) match
              case Left(error) => failure = Some(error)
              case Right(_) =>
                val physical =
                  toUint8Array(logical, requested)
                writePhysical(
                  descriptor,
                  physical,
                  prefix.length.toDouble + payloadOffset.toDouble
                )
                payloadOffset += requested.toLong
          failure.toLeft(())
        finally NodeFs.closeSync(descriptor)
      }

  private def isCompressed(path: String): Boolean =
    path.toLowerCase.endsWith(".gz")

  private def toScalaBytes(bytes: Uint8Array): Array[Byte] =
    val output = new Array[Byte](bytes.length)
    copyToScala(bytes, output, output.length)
    output

  private def copyToScala(
      bytes: Uint8Array,
      output: Array[Byte],
      length: Int
  ): Unit =
    var index = 0
    while index < length do
      output(index) = bytes(index).toByte
      index += 1

  private def toUint8Array(bytes: Array[Byte]): Uint8Array =
    toUint8Array(bytes, bytes.length)

  private def toUint8Array(
      bytes: Array[Byte],
      length: Int
  ): Uint8Array =
    val output = new Uint8Array(length)
    var index = 0
    while index < length do
      output(index) = (bytes(index) & 0xff).toShort
      index += 1
    output

  private def writePhysical(
      descriptor: Int,
      bytes: Uint8Array,
      position: Double
  ): Unit =
    // Node's imperative descriptor API has no typed error return. Failure is
    // caught by protect and translated to NiftiError.IoFailure.
    var written = 0
    while written < bytes.length do
      val count =
        NodeFs.writeSync(
          descriptor,
          bytes,
          written,
          bytes.length - written,
          position + written.toDouble
        )
      if count <= 0 then
        throw new IllegalStateException("Node writeSync made no progress")
      written += count

  private def clampToInt(value: Long): Int =
    math.max(0L, math.min(value, Int.MaxValue.toLong)).toInt

  private def protect[A](
      path: String,
      operation: NiftiOperation
  )(
      body: => Either[NiftiError, A]
  ): Either[NiftiError, A] =
    try body
    catch
      case error: Throwable =>
        Left(
          NiftiError.IoFailure(
            path,
            operation,
            Option(error.getMessage).getOrElse(error.getClass.getName)
          )
        )

@js.native
@JSImport("node:fs",JSImport.Namespace)
private object NodeFs extends js.Object:
  def existsSync(path: String): Boolean = js.native

  def readFileSync(path: String): Uint8Array = js.native

  def openSync(path: String, flags: String): Int = js.native

  def closeSync(descriptor: Int): Unit = js.native

  def readSync(
      descriptor: Int,
      buffer: Uint8Array,
      offset: Int,
      length: Int,
      position: Double
  ): Int = js.native

  def mkdirSync(path: String, options: js.Object): Unit = js.native

  def writeFileSync(path: String, bytes: Uint8Array): Unit = js.native

  def writeSync(
      descriptor: Int,
      buffer: Uint8Array,
      offset: Int,
      length: Int,
      position: Double
  ): Int = js.native

@js.native
@JSImport("node:path",JSImport.Namespace)
private object NodePath extends js.Object:
  def basename(path: String): String = js.native

  def dirname(path: String): String = js.native

  def join(parts: String*): String = js.native

@js.native
@JSImport("node:zlib",JSImport.Namespace)
private object NodeZlib extends js.Object:
  def gunzipSync(bytes: Uint8Array): Uint8Array = js.native

  def gzipSync(bytes: Uint8Array): Uint8Array = js.native
