package image4s.nifti

import image4s.{Axis, AxisKind, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, Grid}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, StandardOpenOption}
import java.nio.channels.FileChannel
import java.lang.management.ManagementFactory
import com.sun.management.UnixOperatingSystemMXBean
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class NiftiIncrementalJvmSuite extends FunSuite:
  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val frame = right(
    Frame.named[D3]("jvm-incremental", convention = CoordinateConvention.RAS)
  )
  private val grid = right(Grid.in(frame)(Vector(3, 2, 2), Affine.identity[D3]))
  private val axes = right(
    NonSpatialAxes.from(Vector(right(Axis.ordinal("map", AxisKind.Batch, 2))))
  )

  test("public JVM writer matches ordinary file bytes and refuses an existing destination"):
    val dir = Files.createTempDirectory("image4s-incremental-jvm-")
    val path = dir.resolve("blocks.nii")
    val full = dir.resolve("resident.nii")
    try
      val o = NiftiWriteOptions
        .forDatatype(NiftiDatatype.Float32)
        .withCoordinateSystem(NiftiCoordinateSystem.Mni152)
      right(Nifti.withScalarWriter(path, grid, axes, o) { w =>
        for
          _ <- w.writeSpatialBlock(Vector(1), Array(11L, 0L, 1L), Array(23.0, 12.0, 13.0))
          _ <- w.writeSpatialSpan(Vector(0), 0L, Array.tabulate(12)(_.toDouble))
          _ <- w.writeSpatialSpan(Vector(1), 2L, Array.tabulate(9)(i => (i + 14).toDouble))
        yield ()
      })
      val image = right(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](3, 2, 2, 2)((x, y, z, t) => x + 3 * y + 6 * z + 12 * t)
        )
      )
      right(Nifti.writeScalar(full, image, o))
      assertEquals(Files.readAllBytes(path).toVector, Files.readAllBytes(full).toVector)
      val before = Files.readAllBytes(path).toVector
      assertEquals(
        Nifti.openScalarWriter(path, grid).left.toOption,
        Some(NiftiError.OutputAlreadyExists(path.toString))
      )
      assertEquals(Files.readAllBytes(path).toVector, before)
      assertEquals(right(Nifti.readHeader(path)).dimensions, Vector(3, 2, 2, 2))
      val bytes = ByteBuffer.wrap(Files.readAllBytes(path)).order(ByteOrder.LITTLE_ENDIAN)
      for i <- 0 until 24 do assertEquals(bytes.getFloat(352 + 4 * i), i.toFloat)
    finally
      val _ = Files.deleteIfExists(path)
      val _ = Files.deleteIfExists(full)
      Files.delete(dir)

  test("public JVM positional writes preserve exact offsets beyond 32-bit byte addressing"):
    val dir = Files.createTempDirectory("image4s-incremental-large-")
    val path = dir.resolve("sparse.nii")
    val large = right(Grid.in(frame)(Vector(32767, 32767, 1), Affine.identity[D3]))
    val samples = 32767L * 32767L
    try
      right(Nifti.withScalarWriter(path, large) { writer =>
        writer.writeSpatialSpan(Vector.empty, samples - 1L, Array(17.5))
      })
      assertEquals(Files.size(path), 352L + samples * 8L)
      val channel = FileChannel.open(path, StandardOpenOption.READ)
      try
        for (offset, expected) <- Vector(352L -> 0.0, (352L + (samples - 1L) * 8L) -> 17.5) do
          val b = ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
          var position = offset
          while b.hasRemaining do
            val n = channel.read(b, position)
            assert(n > 0)
            position += n.toLong
          assertEquals(b.getDouble(0), expected)
      finally channel.close()
    finally
      val _ = Files.deleteIfExists(path)
      Files.delete(dir)

  test(
    "scoped resources close after repeated success and producer exceptions; creation failures are typed"
  ):
    val dir = Files.createTempDirectory("image4s-incremental-close-")
    val path = dir.resolve("stage.nii")
    val blocker = dir.resolve("file-parent")
    val _ = Files.write(blocker, Array[Byte](1))
    try
      // Warm up the exact path before checking the OS descriptor count.
      right(Nifti.withScalarWriter(path, grid)(_ => Right(())))
      Files.delete(path)
      val bean = ManagementFactory.getOperatingSystemMXBean
      def descriptors = bean match
        case unix: UnixOperatingSystemMXBean => Some(unix.getOpenFileDescriptorCount)
        case _ => None
      val before = descriptors
      for i <- 0 until 64 do
        if i % 2 == 0 then
          right(
            Nifti.withScalarWriter(path, grid)(_.writeSpatialSpan(Vector.empty, 0L, Array(1.0)))
          )
        else
          intercept[IllegalArgumentException] {
            Nifti.withScalarWriter(path, grid)(_ =>
              throw new IllegalArgumentException("producer failed")
            )
          }
        Files.delete(path)
      assertEquals(descriptors, before)
      assert(
        Nifti
          .openScalarWriter(blocker.resolve("bad.nii"), grid)
          .left
          .toOption
          .exists(_.isInstanceOf[NiftiError.IoFailure])
      )
      assertEquals(Files.readAllBytes(blocker).toVector, Vector[Byte](1))
    finally
      val _ = Files.deleteIfExists(path)
      Files.delete(blocker)
      Files.delete(dir)
