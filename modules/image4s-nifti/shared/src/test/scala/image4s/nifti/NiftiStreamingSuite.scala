package image4s.nifti

import scala.collection.mutable

import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class NiftiStreamingSuite extends FunSuite:
  test("codec uses bounded chunk APIs and preserves independent checksums"):
    val fileSystem = new ProbedNiftiFileSystem
    val api = new NiftiApi(fileSystem)
    val limits =
      NiftiIoLimits(
        workingBufferBytes = 32,
        maximumPayloadBytes = 1024 * 1024,
        maximumDecodedBytes = 2 * 1024 * 1024,
        maximumExtensionBytes = 1024
      )
    val image = continuousImage(Vector(11, 7, 5), timePoints = 3)
    val options =
      NiftiWriteOptions
        .forDatatype(NiftiDatatype.Float32)
        .withIoLimits(limits)

    niftiRight(api.writeScalar("bounded.nii", image, options))

    assertEquals(fileSystem.writeChunkCalls, 1)
    assertEquals(fileSystem.maximumWriteBuffer, 32)
    fileSystem.resetReadProbe()

    val decoded =
      niftiRight(
        api.readScaledDouble(
          "bounded.nii",
          options = NiftiReadOptions(
            LengthUnit.Millimeter,
            ioLimits = limits
          )
        )
      ).image.value
    val expected = checksum(image.data.elementsIterator)
    val actual = checksum(decoded.data.elementsIterator)

    assertEquals(actual, expected)
    assert(fileSystem.readChunkCalls >= 3)
    assertEquals(fileSystem.readBytesCalls, 0)
    assert(fileSystem.maximumReadBuffer <= limits.workingBufferBytes)

  test("every truncated payload byte boundary fails as typed EOF"):
    val fileSystem = new ProbedNiftiFileSystem
    val api = new NiftiApi(fileSystem)
    val image = continuousImage(Vector(2, 2, 2), timePoints = 1)
    val options =
      NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
    niftiRight(api.writeScalar("complete.nii", image, options))
    val complete = fileSystem.bytes("complete.nii")
    val header = niftiRight(api.readHeader("complete.nii"))

    var cut = header.voxelOffset
    while cut < complete.length do
      fileSystem.put("truncated.nii", complete.take(cut))
      api.readScaledDouble("truncated.nii") match
        case Left(_: NiftiError.UnexpectedEndOfFile) =>
          ()
        case other =>
          fail(s"cut at byte $cut did not fail as typed EOF: $other")
      cut += 1

  test("read and write resource limits fail before payload allocation or output"):
    val fileSystem = new ProbedNiftiFileSystem
    val api = new NiftiApi(fileSystem)
    val image = continuousImage(Vector(4, 3, 2), timePoints = 2)
    niftiRight(api.writeScalar("resource.nii", image))
    val requiredPayload = image.data.size.toLong * 8L
    val payloadLimited =
      NiftiIoLimits.default.copy(
        maximumPayloadBytes = requiredPayload - 1L
      )
    val decodedLimited =
      NiftiIoLimits.default.copy(
        maximumDecodedBytes = image.data.size.toLong * 8L - 1L
      )

    assertEquals(
      api.readScaledDouble(
        "resource.nii",
        NiftiReadOptions(
          LengthUnit.Millimeter,
          ioLimits = payloadLimited
        )
      ),
      Left(
        NiftiError.PayloadResourceLimitExceeded(
          NiftiOperation.ReadPayload,
          requiredPayload,
          requiredPayload - 1L
        )
      )
    )
    assertEquals(
      api.readScaledDouble(
        "resource.nii",
        NiftiReadOptions(
          LengthUnit.Millimeter,
          ioLimits = decodedLimited
        )
      ),
      Left(
        NiftiError.DecodedResourceLimitExceeded(
          requiredPayload,
          requiredPayload - 1L,
          "Double"
        )
      )
    )
    val invalid =
      NiftiIoLimits.default.copy(workingBufferBytes = 0)
    assertEquals(
      api.readHeader("resource.nii", invalid),
      Left(NiftiError.InvalidIoLimit("workingBufferBytes", 0L))
    )

    val writePath = "limited-output.nii"
    val writeOptions =
      NiftiWriteOptions.default.withIoLimits(payloadLimited)
    assertEquals(
      api.writeScalar(writePath, image, writeOptions),
      Left(
        NiftiError.PayloadResourceLimitExceeded(
          NiftiOperation.Write,
          requiredPayload,
          requiredPayload - 1L
        )
      )
    )
    assert(!fileSystem.exists(writePath))

  test("extension bounds are enforced before allocating the declared region"):
    val fileSystem = new ProbedNiftiFileSystem
    val api = new NiftiApi(fileSystem)
    val image = continuousImage(Vector(2, 2, 2), timePoints = 1)
    val extension =
      NiftiExtension
        .create(6, Vector.fill(64)(1.toByte))
        .fold(error => fail(error.message), identity)
    val limits =
      NiftiIoLimits.default.copy(maximumExtensionBytes = 16)
    val options =
      NiftiWriteOptions.default.withIoLimits(limits)

    api.writeScalar(
      "extension-limit.nii",
      image,
      options,
      Vector(extension)
    ) match
      case Left(_: NiftiError.ExtensionResourceLimitExceeded) =>
        ()
      case other =>
        fail(s"expected extension resource failure, got $other")
    assert(!fileSystem.exists("extension-limit.nii"))

  private def continuousImage(
      spatialShape: Vector[Int],
      timePoints: Int
  ) =
    val frame = geometryRight(Frame.named[D3]("streaming"))
    val grid =
      geometryRight(
        Grid.in(frame)(spatialShape, Affine.identity[D3])
      )
    val time =
      imageRight(Axis.create("time", timePoints, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val nx = spatialShape(0)
    val ny = spatialShape(1)
    val nz = spatialShape(2)
    imageRight(
      Sampled.continuous(
        grid,
        axes,
        NDArray.tabulate[Double](nx, ny, nz, timePoints) { (i, j, k, t) =>
          (i + 17 * j + 101 * k + 1009 * t).toDouble
        }
      )
    )

  private final case class Checksum(
      count: Int,
      sum: Double,
      weightedSum: Double
  )

  private def checksum(
      values: Iterator[Double]
  ): Checksum =
    var count = 0
    var sum = 0.0
    var weighted = 0.0
    while values.hasNext do
      val value = values.next()
      sum += value
      weighted += value * (count.toDouble + 1.0)
      count += 1
    Checksum(count, sum, weighted)

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def imageRight[A](
      value: Either[image4s.ImageError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def niftiRight[A](
      value: Either[NiftiError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

private final class ProbedNiftiFileSystem extends NiftiFileSystem[String]:
  private val bytesByPath =
    mutable.Map.empty[String, Array[Byte]]

  var readBytesCalls: Int = 0
  var readChunkCalls: Int = 0
  var writeChunkCalls: Int = 0
  var maximumReadBuffer: Int = 0
  var maximumWriteBuffer: Int = 0

  def put(path: String, bytes: Array[Byte]): Unit =
    bytesByPath.update(path, bytes.clone())

  def bytes(path: String): Array[Byte] =
    bytesByPath(path).clone()

  def resetReadProbe(): Unit =
    readBytesCalls = 0
    readChunkCalls = 0
    maximumReadBuffer = 0

  def show(path: String): String =
    path

  def fileName(path: String): String =
    path

  def sibling(path: String, fileName: String): String =
    fileName

  def exists(path: String): Boolean =
    bytesByPath.contains(path)

  def ioStrategy(_path: String): NiftiIoStrategy =
    NiftiIoStrategy.BoundedStreaming

  def readBytes(
      path: String,
      operation: NiftiOperation
  ): Either[NiftiError, Array[Byte]] =
    readBytesCalls += 1
    bytesByPath
      .get(path)
      .map(value => Right(value.clone()))
      .getOrElse(
        Left(NiftiError.IoFailure(path, operation, "missing"))
      )

  override def readUpTo(
      path: String,
      operation: NiftiOperation,
      maximumBytes: Int
  ): Either[NiftiError, NiftiBoundedRead] =
    bytesByPath
      .get(path)
      .map { value =>
        val length = math.min(value.length, maximumBytes)
        Right(
          NiftiBoundedRead(
            java.util.Arrays.copyOf(value, length),
            value.length <= maximumBytes
          )
        )
      }
      .getOrElse(
        Left(NiftiError.IoFailure(path, operation, "missing"))
      )

  override def readChunks(
      path: String,
      operation: NiftiOperation,
      startOffset: Long,
      byteCount: Long,
      workingBufferBytes: Int
  )(
      consume: (Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    readChunkCalls += 1
    maximumReadBuffer = math.max(maximumReadBuffer, workingBufferBytes)
    bytesByPath.get(path) match
      case None =>
        Left(NiftiError.IoFailure(path, operation, "missing"))
      case Some(source) if startOffset + byteCount > source.length.toLong =>
        Left(
          NiftiError.UnexpectedEndOfFile(
            operation,
            math
              .min(
                startOffset + byteCount,
                Int.MaxValue.toLong
              )
              .toInt,
            source.length
          )
        )
      case Some(source) =>
        val buffer =
          new Array[Byte](
            math.min(workingBufferBytes.toLong, byteCount).toInt
          )
        var offset = 0L
        var failure: Option[NiftiError] = None
        while offset < byteCount && failure.isEmpty do
          val length =
            math.min(buffer.length.toLong, byteCount - offset).toInt
          System.arraycopy(
            source,
            startOffset.toInt + offset.toInt,
            buffer,
            0,
            length
          )
          consume(buffer, length) match
            case Left(error) => failure = Some(error)
            case Right(_) => offset += length.toLong
        failure.toLeft(())

  def writeBytes(
      path: String,
      bytes: Array[Byte]
  ): Either[NiftiError, Unit] =
    put(path, bytes)
    Right(())

  override def writeChunks(
      path: String,
      prefix: Array[Byte],
      payloadBytes: Long,
      workingBufferBytes: Int
  )(
      fill: (Long, Array[Byte], Int) => Either[NiftiError, Unit]
  ): Either[NiftiError, Unit] =
    writeChunkCalls += 1
    maximumWriteBuffer = math.max(maximumWriteBuffer, workingBufferBytes)
    val output =
      new Array[Byte]((prefix.length.toLong + payloadBytes).toInt)
    System.arraycopy(prefix, 0, output, 0, prefix.length)
    val buffer =
      new Array[Byte](
        math.min(workingBufferBytes.toLong, payloadBytes).toInt
      )
    var offset = 0L
    var failure: Option[NiftiError] = None
    while offset < payloadBytes && failure.isEmpty do
      val length =
        math.min(buffer.length.toLong, payloadBytes - offset).toInt
      fill(offset, buffer, length) match
        case Left(error) => failure = Some(error)
        case Right(_) =>
          System.arraycopy(
            buffer,
            0,
            output,
            prefix.length + offset.toInt,
            length
          )
          offset += length.toLong
    failure match
      case Some(error) => Left(error)
      case None =>
        put(path, output)
        Right(())
