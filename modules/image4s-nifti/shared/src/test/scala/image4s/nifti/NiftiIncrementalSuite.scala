package image4s.nifti

import image4s.{Axis, AxisKind, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, Grid, LengthUnit}
import java.nio.{ByteBuffer, ByteOrder}
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import scala.collection.mutable

final class NiftiIncrementalSuite extends FunSuite:
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = right(
    Frame
      .named[D3]("incremental", unit = LengthUnit.Millimeter, convention = CoordinateConvention.RAS)
  )
  private val affine = right(
    Affine.fromRowMajor[D3](
      Vector(
        2.0, 0.25, 0.0, -12.0, 0.0, 3.0, 0.0, 8.0, 0.0, 0.0, 4.0, -5.0, 0.0, 0.0, 0.0, 1.0
      )
    )
  )
  private def grid(shape: Vector[Int] = Vector(3, 2, 2)) = right(Grid.in(frame)(shape, affine))
  private def axes(extents: Int*): NonSpatialAxes = right(
    NonSpatialAxes.from(extents.zipWithIndex.map { (n, i) =>
      right(Axis.ordinal(s"axis$i", AxisKind.Batch, n))
    })
  )
  private def options(dt: NiftiDatatype) = NiftiWriteOptions
    .forDatatype(dt)
    .withIoLimits(NiftiIoLimits.default.copy(workingBufferBytes = 17))

  test(
    "produced blocks match ordinary codec bytes and independent NIfTI order for every scalar datatype"
  ):
    for dt <- NiftiDatatype.values do
      val fs = new IncrementalTestFileSystem
      val api = new NiftiApi(fs)
      val g = grid()
      val a = axes(3)
      val data = NDArray.tabulate[Double](3, 2, 2, 3)((x, y, z, t) => x + 3 * y + 6 * z + 12 * t)
      val image = right(Sampled.continuous(g, a, data))
      val o = options(dt).withCoordinateSystem(NiftiCoordinateSystem.Mni152)
      val ext = Vector(right(NiftiExtension.create(6, Vector[Byte](4, 5, 6))))
      right(api.writeScalar("resident.nii", image, o, ext))
      val writer = right(api.openScalarWriter("blocks.nii", g, a, o, ext))
      assert(writer.grid eq g)
      assert(writer.axes eq a)
      for t <- Vector(2, 0, 1) do
        right(
          writer.writeSpatialSpan(Vector(t), 6L, Array.tabulate(6)(i => (i + 6 + 12 * t).toDouble))
        )
        right(writer.writeSpatialSpan(Vector(t), 0L, Array.tabulate(6)(i => (i + 12 * t).toDouble)))
      right(writer.close())
      assertEquals(fs.content("blocks.nii").toVector, fs.content("resident.nii").toVector)
      val bytes = ByteBuffer.wrap(fs.content("blocks.nii")).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(bytes.getInt(0), 348)
      assertEquals(bytes.getShort(40).toInt, 4)
      assertEquals(bytes.getShort(254).toInt, 4)
      assertEquals(bytes.getFloat(280), 2.0f)
      assertEquals(bytes.getFloat(292), -12.0f)
      assertEquals(bytes.get(123).toInt & 7, 2)
      for i <- 0 until 36 do
        val offset = bytes.getFloat(108).toInt + i * dt.bitsPerValue / 8
        val actual = dt match
          case NiftiDatatype.UInt8 => (bytes.get(offset) & 255).toDouble
          case NiftiDatatype.Int16 => bytes.getShort(offset).toDouble
          case NiftiDatatype.Int32 => bytes.getInt(offset).toDouble
          case NiftiDatatype.Float32 => bytes.getFloat(offset).toDouble
          case NiftiDatatype.Float64 => bytes.getDouble(offset)
        assertEquals(actual, i.toDouble)
      assert(fs.maximumWrite <= 16)

  test("sparse and repeated samples use explicit first-axis-fastest non-spatial coordinates"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val o = right(NiftiWriteOptions.create(NiftiDatatype.Int16, 2.0, 10.0))
    val w = right(api.openScalarWriter("sparse.nii", grid(), axes(2, 3), o))
    right(w.writeSpatialBlock(Vector(1, 2), Array(11L, 1L, 2L, 1L), Array(30.0, 14.0, 16.0, 18.0)))
    right(w.close())
    val b = ByteBuffer.wrap(fs.content("sparse.nii")).order(ByteOrder.LITTLE_ENDIAN)
    assertEquals(b.getShort(352 + 2 * ((1 + 2 * 2) * 12 + 11)).toInt, 10)
    assertEquals(b.getShort(352 + 2 * ((1 + 2 * 2) * 12 + 1)).toInt, 4)
    assertEquals(b.getShort(352).toInt, 0) // stored zero decodes to the declared intercept

  test("block validation is complete before physical writes and refusals are recoverable"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val w =
      right(api.openScalarWriter("validate.nii", grid(), axes(2), options(NiftiDatatype.UInt8)))
    val invalid = Vector(
      () => w.writeSpatialSpan(Vector(0), 0L, Array(1.0, 2.5)),
      () => w.writeSpatialSpan(Vector(0), Long.MaxValue, Array(1.0)),
      () => w.writeSpatialSpan(Vector(0), -1L, Array(1.0)),
      () => w.writeSpatialSpan(Vector(0), 11L, Array(1.0, 2.0)),
      () => w.writeSpatialSpan(Vector(2), 0L, Array(1.0)),
      () => w.writeSpatialSpan(Vector(-1), 0L, Array(1.0)),
      () => w.writeSpatialSpan(Vector.empty, 0L, Array(1.0)),
      () => w.writeSpatialBlock(Vector(0), Array(0L, 99L), Array(1.0, 2.0)),
      () => w.writeSpatialBlock(Vector(0), Array(0L), Array(1.0, 2.0))
    )
    invalid.foreach { call =>
      assert(call().isLeft)
      assertEquals(fs.writes.size, 0)
    }
    w.writeSpatialSpan(Vector(1), 7L, Array(3.5)) match
      case Left(e: NiftiError.ValueNotRepresentable) =>
        assertEquals(e.logicalIndex, Vector(1, 0, 1, 1))
      case other => fail(other.toString)
    right(w.writeSpatialSpan(Vector(1), 0L, Array(3.0)))
    right(w.writeSpatialSpan(Vector(1), 12L, Array.emptyDoubleArray))
    right(w.close())
    right(w.close())
    assertEquals(fs.closeCount, 1)
    assertEquals(w.writeSpatialSpan(Vector(0), 0L, Array(1.0)), Left(NiftiError.OutputClosed))

  test(
    "all conversion policies retain ordinary-writer behavior including overflow and non-finite values"
  ):
    for dt <- NiftiDatatype.values do
      for value <- Vector(Double.NaN, Double.PositiveInfinity, -1.0, 1.5, 256.0, 1e300) do
        val fs = new IncrementalTestFileSystem
        val api = new NiftiApi(fs)
        val g = grid(Vector(1, 1, 1))
        val image = right(
          Sampled.continuous(
            g,
            NonSpatialAxes.empty,
            NDArray.tabulate[Double](1, 1, 1)((_, _, _) => value)
          )
        )
        val o =
          right(NiftiWriteOptions.create(dt, 2.0, 10.0, NiftiIntegerConversion.RoundToNearestEven))
        val full = api.writeScalar("full.nii", image, o)
        val w = right(api.openScalarWriter("incremental.nii", g, options = o))
        val block = w.writeSpatialSpan(Vector.empty, 0L, Array(value))
        (block, full) match
          case (
                Left(a: NiftiError.ValueNotRepresentable),
                Left(b: NiftiError.ValueNotRepresentable)
              ) =>
            assertEquals(a.logicalIndex, b.logicalIndex)
            assertEquals(a.datatype, b.datatype)
            assertEquals(a.problem, b.problem)
            assert(a.value == b.value || (a.value.isNaN && b.value.isNaN))
            assert(
              a.encodedValue == b.encodedValue || (a.encodedValue.isNaN && b.encodedValue.isNaN)
            )
          case (Right(_), Right(_)) => ()
          case other => fail(other.toString)
        right(w.close())
        if full.isRight then
          assertEquals(fs.content("incremental.nii").toVector, fs.content("full.nii").toVector)

  test("finite values whose scaling overflows are rejected by both Float32 writer APIs"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val g = grid(Vector(1, 1, 1))
    val o = right(NiftiWriteOptions.create(NiftiDatatype.Float32, 1e-40, 0.0))
    val value = Double.MaxValue
    val image = right(
      Sampled.continuous(
        g,
        NonSpatialAxes.empty,
        NDArray.tabulate[Double](1, 1, 1)((_, _, _) => value)
      )
    )
    val full = api.writeScalar("overflow-full.nii", image, o)
    val w = right(api.openScalarWriter("overflow-block.nii", g, options = o))
    val block = w.writeSpatialSpan(Vector.empty, 0L, Array(value))
    assertEquals(block.left.toOption, full.left.toOption)
    block match
      case Left(e: NiftiError.ValueNotRepresentable) =>
        assertEquals(e.problem, NiftiValueProblem.FloatingOverflow)
      case other => fail(other.toString)
    assertEquals(fs.writes.size, 0)
    right(w.close())

  test("physical failure closes and poisons the output and retains a simultaneous close failure"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val w =
      right(api.openScalarWriter("failure.nii", grid(), options = options(NiftiDatatype.Float64)))
    fs.failAt = Some(2)
    fs.closeFailure = true
    val result = w.writeSpatialSpan(Vector.empty, 0L, Array.fill(6)(2.0))
    assert(result.left.toOption.exists(_.isInstanceOf[NiftiError.OutputCloseFailure]))
    assertEquals(fs.closeCount, 1)
    assertEquals(fs.writes.size, 1)
    assertEquals(w.close(), result)
    assertEquals(w.writeSpatialSpan(Vector.empty, 0L, Array(2.0)), result)

  test("scoped callbacks close on success, typed error and thrown exception"):
    for mode <- 0 until 3 do
      val fs = new IncrementalTestFileSystem
      val api = new NiftiApi(fs)
      def run() = api.withScalarWriter("scoped.nii", grid()) { _ =>
        mode match
          case 0 => Right(4)
          case 1 => Left(NiftiError.InvalidOutputBlock("producer failed"))
          case _ => throw new IllegalStateException("producer threw")
      }
      if mode == 2 then intercept[IllegalStateException](run())
      else assertEquals(run().isRight, mode == 0)
      assertEquals(fs.closeCount, 1)

  test("dimensions, overflow, limits and unsupported modes refuse creation"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    assert(api.openScalarWriter("rank.nii", grid(), axes(1, 1, 1, 1, 1)).isLeft)
    assert(api.openScalarWriter("dimension.nii", grid(Vector(32768, 1, 1))).isLeft)
    assert(
      api
        .openScalarWriter(
          "overflow.nii",
          grid(Vector(32767, 32767, 32767)),
          axes(32767, 32767, 32767, 32767)
        )
        .left
        .toOption
        .exists(_.isInstanceOf[NiftiError.WriteSizeOverflow])
    )
    assert(
      api
        .openScalarWriter(
          "limit.nii",
          grid(),
          options = NiftiWriteOptions.default
            .withIoLimits(NiftiIoLimits.default.copy(maximumPayloadBytes = 8))
        )
        .isLeft
    )
    assert(
      api
        .openScalarWriter(
          "working.nii",
          grid(),
          options = NiftiWriteOptions.default
            .withIoLimits(NiftiIoLimits.default.copy(workingBufferBytes = 0))
        )
        .isLeft
    )
    for suffix <- Vector(".nii.gz", ".hdr", ".img", ".hdr.gz", ".txt") do
      assert(
        api
          .openScalarWriter("unsupported" + suffix, grid())
          .left
          .toOption
          .exists(_.isInstanceOf[NiftiError.UnsupportedIncrementalOutput])
      )
    assertEquals(fs.created, 0)

  test("incremental output requires an explicit RAS frame without changing legacy write behavior"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val unspecified = right(Frame.named[D3]("unspecified"))
    val g = right(Grid.in(unspecified)(Vector(1, 1, 1), Affine.identity[D3]))
    assertEquals(
      api.openScalarWriter("new.nii", g).left.toOption,
      Some(NiftiError.FrameConventionMismatch(CoordinateConvention.Unspecified))
    )
    assertEquals(fs.created, 0)
    val image = right(
      Sampled.continuous(
        g,
        NonSpatialAxes.empty,
        NDArray.tabulate[Double](1, 1, 1)((_, _, _) => 2.0)
      )
    )
    right(api.writeScalar("legacy.nii", image))

  test("payload addressing beyond Int range is checked and uses exact Long arithmetic"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    val g = grid(Vector(32767, 32767, 2))
    val o = NiftiWriteOptions.default.withIoLimits(
      NiftiIoLimits.default.copy(maximumPayloadBytes = Long.MaxValue)
    )
    val w = right(api.openScalarWriter("large.nii", g, axes(3), o))
    val spatial = 32767L * 32767L * 2L
    right(w.writeSpatialSpan(Vector(2), spatial - 1L, Array(7.5)))
    assertEquals(fs.writes.last._1, 352L + (3L * spatial - 1L) * 8L)
    assertEquals(fs.writes.last._2, 8)
    right(w.close())

  test("coordinate declarations survive option builders with historical default preserved"):
    val fs = new IncrementalTestFileSystem
    val api = new NiftiApi(fs)
    assertEquals(
      NiftiWriteOptions.default.coordinateSystem,
      NiftiCoordinateSystem.ScannerAnatomical
    )
    for system <- NiftiCoordinateSystem.values do
      val initial = NiftiWriteOptions.default.withCoordinateSystem(system)
      val sampled = right(initial.withNonSpatialSampling(Vector(0.5), NiftiTemporalUnit.Second))
      val limited = sampled.withIoLimits(NiftiIoLimits.default)
      val created = right(
        NiftiWriteOptions.create(
          NiftiDatatype.Float64,
          1,
          0,
          nonSpatialPixelDimensions = Vector(0.5),
          temporalUnit = NiftiTemporalUnit.Second,
          coordinateSystem = system
        )
      )
      assertEquals(limited, created)
      assertEquals(limited.hashCode, created.hashCode)
      val path = s"system-${system.code}.nii"
      right(right(api.openScalarWriter(path, grid(), axes(2), limited)).close())
      val bytes = ByteBuffer.wrap(fs.content(path)).order(ByteOrder.LITTLE_ENDIAN)
      assertEquals(bytes.getShort(254).toInt, system.code)
      assertEquals(bytes.getFloat(92), 0.5f)
      assertEquals(bytes.get(123).toInt, 10)

private[nifti] final class IncrementalTestFileSystem extends NiftiFileSystem[String]:
  val content = mutable.Map.empty[String, Array[Byte]]
  val writes = mutable.ArrayBuffer.empty[(Long, Int)]
  var created = 0
  var closeCount = 0
  var maximumWrite = 0
  var failAt: Option[Int] = None
  var closeFailure = false
  private var attempts = 0
  def show(path: String): String = path
  def fileName(path: String): String = path
  def sibling(_path: String, name: String): String = name
  def exists(path: String): Boolean = content.contains(path)
  def ioStrategy(_path: String): NiftiIoStrategy = NiftiIoStrategy.BoundedStreaming
  def readBytes(path: String, _operation: NiftiOperation): Either[NiftiError, Array[Byte]] = Right(
    content(path)
  )
  def writeBytes(path: String, bytes: Array[Byte]): Either[NiftiError, Unit] =
    content(path) = bytes.clone()
    Right(())
  override def createSeekable(
      path: String,
      prefix: Array[Byte],
      payloadBytes: Long
  ): Either[NiftiError, NiftiSeekableOutput] =
    if exists(path) then Left(NiftiError.OutputAlreadyExists(path))
    else
      created += 1
      val total = prefix.length.toLong + payloadBytes
      content(path) =
        if total < 1000000L then new Array[Byte](total.toInt) else new Array[Byte](prefix.length)
      System.arraycopy(prefix, 0, content(path), 0, prefix.length)
      Right(new NiftiSeekableOutput:
        def writeAt(offset: Long, bytes: Array[Byte], length: Int): Either[NiftiError, Unit] =
          attempts += 1
          if failAt.contains(attempts) then
            Left(NiftiError.IoFailure(path, NiftiOperation.Write, "injected write failure"))
          else
            writes += ((offset, length))
            maximumWrite = math.max(maximumWrite, length)
            if offset + length <= content(path).length then
              System.arraycopy(bytes, 0, content(path), offset.toInt, length)
            Right(())
        def close(): Either[NiftiError, Unit] =
          closeCount += 1
          if closeFailure then
            Left(NiftiError.IoFailure(path, NiftiOperation.Write, "injected close failure"))
          else Right(()))
