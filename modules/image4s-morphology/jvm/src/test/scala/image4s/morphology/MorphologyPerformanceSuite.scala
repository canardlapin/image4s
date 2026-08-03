package image4s.morphology

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.ops.OpError
import image4s.ops.Radius
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class MorphologyPerformanceSuite extends FunSuite:
  private var retained: AnyRef = null

  test("prepared Boolean dilation avoids per-sample allocation"):
    val edge = 256
    val samples = edge * edge
    val frame = geometryRight(Frame.named[D2]("morphology-allocation"))
    val grid = geometryRight(Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val input =
      imageRight(
        Sampled.mask(
          space,
          NDArray.tabulate[Boolean](edge, edge)((row, column) => (row + 3 * column) % 11 == 0)
        )
      )
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val plan = opsRight(BinaryMorphology.prepareDilate(input, element))
    val run = () => retained = opsRight(plan.run(input)).asInstanceOf[AnyRef]

    Vector.fill(5)(run())
    val allocated = Vector.fill(7)(allocatedBytes(run())).sorted.apply(3)
    val outputBytes = samples.toLong
    val limit = outputBytes + 128L * 1024L

    assert(
      allocated <= limit,
      s"prepared Boolean dilation allocated $allocated B; limit=$limit"
    )
    println(
      s"IMG-MORPHOLOGY JVM allocation: operation=prepared-dilate, " +
        s"samples=$samples, allocated=$allocated B"
    )
    assert(retained != null)

  // JDK 17 exposes only Thread.getId; newer JDKs deprecate it in favor of threadId.
  @scala.annotation.nowarn("cat=deprecation")
  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("thread allocation accounting is unavailable")
    val thread = Thread.currentThread().getId()
    val before = bean.getThreadAllocatedBytes(thread)
    body
    bean.getThreadAllocatedBytes(thread) - before

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
