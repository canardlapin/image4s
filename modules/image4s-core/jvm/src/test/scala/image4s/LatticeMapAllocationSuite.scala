package image4s

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class LatticeMapAllocationSuite extends FunSuite:
  @volatile private var sink: Long = 0L

  test("exact views allocate only small metadata independent of sample count"):
    val frame = geometryRight(Frame.named[D3]("view-allocation"))
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(48, 48, 32),
          Affine.identity[D3]
        )
      )
    val time = imageRight(Axis.create("time", 4, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.zeros[Double](48, 48, 32, 4)
        )
      )
    val maps =
      Vector(
        "crop" ->
          imageRight(
            LatticeMap.crop[D3](
              grid.shape,
              Vector(4, 5, 3),
              Vector(36, 34, 25)
            )
          ),
        "flip" ->
          imageRight(LatticeMap.flip[D3](grid.shape, 1)),
        "permutation" ->
          imageRight(
            LatticeMap.permute[D3](grid.shape, Vector(2, 0, 1))
          ),
        "stride" ->
          imageRight(
            LatticeMap.stride[D3](grid.shape, Vector(2, 3, 2))
          )
      )

    maps.foreach { case (_, map) =>
      var warmup = 0
      while warmup < 50 do
        consume(imageRight(source.view(map)))
        warmup += 1
    }

    maps.foreach { case (name, map) =>
      val samples =
        Vector
          .fill(15) {
            allocatedBytes {
              consume(imageRight(source.view(map)))
            }
          }
          .sorted
      val median = samples(samples.size / 2)
      assert(
        median <= 128L * 1024L,
        s"$name exact view allocated $median bytes; a view must not copy " +
          s"${source.data.size} stored values"
      )
      println(
        s"IMG-EXACT-VIEW allocation: operation=$name, " +
          s"sourceSamples=${source.data.size}, allocated=$median B"
      )
    }

  private def consume(
      value: Sampled[?, ?, ?, ?]
  ): Unit =
    sink = sink + value.data.size.toLong + value.grid.shape.sum.toLong

  // JDK 17 exposes only Thread.getId; newer JDKs deprecate it in favor of threadId.
  @scala.annotation.nowarn("cat=deprecation")
  private def allocatedBytes(
      body: => Unit
  ): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("this JVM does not expose per-thread allocation accounting")
    val threadId = Thread.currentThread().getId()
    val before = bean.getThreadAllocatedBytes(threadId)
    body
    bean.getThreadAllocatedBytes(threadId) - before

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
