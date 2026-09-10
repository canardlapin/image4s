package image4s.nifti

import image4s.{Axis, AxisKind, NonSpatialAxes}
import image4s.geometry.{Affine, CoordinateConvention, D3, Frame, Grid}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}

/** Run this public-API probe in a separate JVM with -Xmx64m. No whole-image NDArray is created. */
object NiftiIncrementalHeapProbe:
  private def checked[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalStateException(x.toString), identity)
  def main(args: Array[String]): Unit =
    require(Runtime.getRuntime.maxMemory() <= 64L * 1024L * 1024L, "run with -Xmx64m")
    require(args.length == 1, "supply a new output .nii path")
    val path = Path.of(args(0))
    val frame = checked(Frame.named[D3]("heap-proof", convention = CoordinateConvention.RAS))
    val grid = checked(Grid.in(frame)(Vector(200, 200, 100), Affine.identity[D3]))
    val axes = checked(
      NonSpatialAxes.from(Vector(checked(Axis.ordinal("map", AxisKind.Batch, 10))))
    )
    val options = NiftiWriteOptions.forDatatype(NiftiDatatype.Float32)
    val values = new Array[Double](8000)
    val started = System.nanoTime()
    checked(Nifti.withScalarWriter(path, grid, axes, options) { writer =>
      var start = 0L
      var failure: Option[NiftiError] = None
      while start < writer.spatialSize && failure.isEmpty do
        var map = 9
        while map >= 0 && failure.isEmpty do
          var i = 0
          while i < values.length do
            values(i) = map * 100.0 + ((start + i) % 97L).toDouble
            i += 1
          writer.writeSpatialSpan(Vector(map), start, values) match
            case Left(e) => failure = Some(e)
            case Right(_) => ()
          map -= 1
        start += values.length.toLong
      failure.toLeft(())
    })
    require(Files.size(path) == 160000352L)
    val channel = FileChannel.open(path, StandardOpenOption.READ)
    var count = 0L
    var sum = 0.0
    try
      val header = ByteBuffer.allocate(352).order(ByteOrder.LITTLE_ENDIAN)
      while header.hasRemaining do require(channel.read(header) > 0)
      require(header.getInt(0) == 348 && header.getFloat(108) == 352.0f)
      require(header.getShort(40) == 4 && header.getShort(48) == 10)
      val buffer = ByteBuffer.allocate(65536).order(ByteOrder.LITTLE_ENDIAN)
      var remaining = 160000000L
      while remaining > 0L do
        val _ = buffer.clear()
        val _ = buffer.limit(math.min(buffer.capacity().toLong, remaining).toInt)
        while buffer.hasRemaining do require(channel.read(buffer) > 0)
        val _ = buffer.flip()
        while buffer.hasRemaining do
          val actual = buffer.getFloat().toDouble
          val map = count / 4000000L
          val spatial = count % 4000000L
          val expected = map * 100.0 + (spatial % 97L).toDouble
          require(actual == expected, s"wrong value at $count: $actual != $expected")
          sum += actual
          count += 1
        remaining = 160000000L - count * 4L
    finally channel.close()
    require(count == 40000000L)
    println(
      s"PASS heapBytes=${Runtime.getRuntime.maxMemory()} payloadBytes=160000000 values=$count checksum=$sum seconds=${(System.nanoTime() - started) / 1e9}"
    )
