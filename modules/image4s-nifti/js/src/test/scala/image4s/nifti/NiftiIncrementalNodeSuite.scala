package image4s.nifti

import image4s.{Axis, AxisKind, NonSpatialAxes, Sampled}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, Grid}
import java.nio.{ByteBuffer, ByteOrder}
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.typedarray.Uint8Array

final class NiftiIncrementalNodeSuite extends FunSuite:
  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val frame = right(
    Frame.named[D3]("node-incremental", convention = CoordinateConvention.RAS)
  )
  private val grid = right(Grid.in(frame)(Vector(3, 2, 2), Affine.identity[D3]))
  private val axes = right(
    NonSpatialAxes.from(Vector(right(Axis.ordinal("map", AxisKind.Batch, 2))))
  )
  private def bytes(path: String): Vector[Byte] =
    val b = IncrementalNodeFs.readFileSync(path)
    Vector.tabulate(b.length)(i => b(i).toByte)

  test("public Node writer matches ordinary file bytes and refuses an existing destination"):
    val dir =
      IncrementalNodeFs.mkdtempSync(IncrementalNodeOs.tmpdir() + "/image4s-incremental-node-")
    val path = dir + "/blocks.nii"
    val full = dir + "/resident.nii"
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
      assertEquals(bytes(path), bytes(full))
      val before = bytes(path)
      assertEquals(
        Nifti.openScalarWriter(path, grid).left.toOption,
        Some(NiftiError.OutputAlreadyExists(path))
      )
      assertEquals(bytes(path), before)
      assertEquals(right(Nifti.readHeader(path)).dimensions, Vector(3, 2, 2, 2))
      val b = ByteBuffer.wrap(bytes(path).toArray).order(ByteOrder.LITTLE_ENDIAN)
      for i <- 0 until 24 do assertEquals(b.getFloat(352 + 4 * i), i.toFloat)
    finally IncrementalNodeFs.rmSync(dir, js.Dynamic.literal(recursive = true, force = true))

  test("public Node positional writes preserve exact offsets beyond 32-bit byte addressing"):
    val dir =
      IncrementalNodeFs.mkdtempSync(IncrementalNodeOs.tmpdir() + "/image4s-incremental-large-")
    val path = dir + "/sparse.nii"
    val large = right(Grid.in(frame)(Vector(32767, 32767, 1), Affine.identity[D3]))
    val samples = 32767L * 32767L
    try
      right(Nifti.withScalarWriter(path, large) { writer =>
        writer.writeSpatialSpan(Vector.empty, samples - 1L, Array(17.5))
      })
      assertEquals(
        IncrementalNodeFs.statSync(path).size.asInstanceOf[Double],
        (352L + samples * 8L).toDouble
      )
      val fd = IncrementalNodeFs.openSync(path, "r")
      try
        for (offset, expected) <- Vector(352L -> 0.0, (352L + (samples - 1L) * 8L) -> 17.5) do
          val raw = new Uint8Array(8)
          var count = 0
          while count < 8 do
            val n = IncrementalNodeFs.readSync(fd, raw, count, 8 - count, offset.toDouble + count)
            assert(n > 0)
            count += n
          val b = ByteBuffer
            .wrap(Array.tabulate[Byte](8)(i => raw(i).toByte))
            .order(ByteOrder.LITTLE_ENDIAN)
          assertEquals(b.getDouble(0), expected)
      finally IncrementalNodeFs.closeSync(fd)
    finally IncrementalNodeFs.rmSync(dir, js.Dynamic.literal(recursive = true, force = true))

  test(
    "Node scoped resources close after success and producer exceptions; creation failures are typed"
  ):
    val dir =
      IncrementalNodeFs.mkdtempSync(IncrementalNodeOs.tmpdir() + "/image4s-incremental-close-")
    val path = dir + "/stage.nii"
    val blocker = dir + "/file-parent"
    IncrementalNodeFs.writeFileSync(blocker, new Uint8Array(1))
    try
      right(Nifti.withScalarWriter(path, grid)(_ => Right(())))
      IncrementalNodeFs.unlinkSync(path)
      def descriptors =
        if IncrementalNodeFs.existsSync("/dev/fd") then
          Some(IncrementalNodeFs.readdirSync("/dev/fd").length)
        else None
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
        IncrementalNodeFs.unlinkSync(path)
      assertEquals(descriptors, before)
      assert(
        Nifti
          .openScalarWriter(blocker + "/bad.nii", grid)
          .left
          .toOption
          .exists(_.isInstanceOf[NiftiError.IoFailure])
      )
      assertEquals(bytes(blocker), Vector[Byte](0))
    finally IncrementalNodeFs.rmSync(dir, js.Dynamic.literal(recursive = true, force = true))

@js.native
@JSImport("node:fs", JSImport.Namespace)
private object IncrementalNodeFs extends js.Object:
  def openSync(path: String, flags: String): Int = js.native
  def closeSync(fd: Int): Unit = js.native
  def statSync(path: String): js.Dynamic = js.native
  def readSync(fd: Int, buffer: Uint8Array, offset: Int, length: Int, position: Double): Int =
    js.native
  def readFileSync(path: String): Uint8Array = js.native
  def writeFileSync(path: String, bytes: Uint8Array): Unit = js.native
  def existsSync(path: String): Boolean = js.native
  def readdirSync(path: String): js.Array[String] = js.native
  def mkdtempSync(prefix: String): String = js.native
  def rmSync(path: String, options: js.Object): Unit = js.native
  def unlinkSync(path: String): Unit = js.native

@js.native
@JSImport("node:os", JSImport.Namespace)
private object IncrementalNodeOs extends js.Object:
  def tmpdir(): String = js.native
