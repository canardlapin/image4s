package image4s.intaglio

import java.lang.management.ManagementFactory

import _root_.intaglio.DisplayWindow
import _root_.intaglio.RasterImage
import com.sun.management.ThreadMXBean
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class DisplayBridgePerformanceSuite extends FunSuite:
  @volatile private var retained: RasterImage = null

  test("raster packing allocates only its primitive output"):
    val width = 256
    val height = 256
    val frame = geometryRight(Frame.named[D2]("display-pack"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(width, height), Affine.identity[D2])
      )
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](width, height) { (x, y) =>
            (x + width * y).toDouble
          }
        )
      )
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, (width * height - 1).toDouble))
    val render = () =>
      retained = DisplayBridge.renderRaster(image, plan)

    Vector.fill(3)(render())
    val allocations = Vector.fill(7)(allocatedBytes(render())).sorted
    val median = allocations(allocations.size / 2)
    val outputBytes = width.toLong * height.toLong * 4L

    assert(
      median <= outputBytes + 64L * 1024L,
      s"raster pack allocated $median bytes; output requires $outputBytes bytes"
    )
    assertEquals(retained.width, width)
    assertEquals(retained.height, height)

  private def allocatedBytes(
      body: => Unit
  ): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean
            if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then
            value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("this JVM does not expose per-thread allocation accounting")
    val threadId = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(threadId)
    body
    val after = bean.getThreadAllocatedBytes(threadId)
    assert(retained != null)
    after - before

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value.fold(error => fail(error.message), identity)
