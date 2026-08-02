package image4s.filter

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
import image4s.ops.Border
import image4s.ops.FilterExtent
import image4s.ops.OpError
import image4s.ops.SpatialSigma
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

final class FilterPerformanceSuite extends FunSuite:
  private var retained: AnyRef = null

  test("preserving and promoting Gaussian passes avoid per-sample boxing"):
    val edge = 256
    val samples = edge * edge
    val frame = geometryRight(Frame.named[D2]("filter-allocation"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(edge, edge), Affine.identity[D2])
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val floatImage =
      imageRight(
        Sampled.continuous[Float, Rank[2]](
          space,
          NDArray.tabulate[Float](edge, edge) { (row, column) =>
            ((row + column) % 31).toFloat
          }
        )
      )
    val byteImage =
      imageRight(
        Sampled.continuous[Byte, Rank[2]](
          space,
          NDArray.tabulate[Byte](edge, edge) { (row, column) =>
            ((row + column) % 127).toByte
          }
        )
      )
    val sigma = opsRight(SpatialSigma.samples[D2](1.5))
    val extent = FilterExtent.same(Border.Replicate)
    val preserving = () =>
      retained =
        opsRight(floatImage.gaussianBlur(sigma, extent)).asInstanceOf[AnyRef]
    val promoting = () =>
      retained =
        opsRight(byteImage.gaussianBlurTo[Float](sigma, extent))
          .asInstanceOf[AnyRef]
    val preparedPlan =
      opsRight(Gaussian.prepare(floatImage, sigma, extent))
    val prepared = () =>
      retained = opsRight(preparedPlan.run(floatImage)).asInstanceOf[AnyRef]

    Vector.fill(3) {
      preserving()
      promoting()
      prepared()
    }
    val preservingMedian =
      Vector.fill(7)(allocatedBytes(preserving())).sorted.apply(3)
    val promotingMedian =
      Vector.fill(7)(allocatedBytes(promoting())).sorted.apply(3)
    val preparedMedian =
      Vector.fill(7)(allocatedBytes(prepared())).sorted.apply(3)
    val preservingLimit = samples.toLong * 12L + 256L * 1024L
    val promotingLimit = samples.toLong * 16L + 256L * 1024L
    val preparedLimit = samples.toLong * 5L + 128L * 1024L

    assert(
      preservingMedian <= preservingLimit,
      s"Float-preserving Gaussian allocated $preservingMedian bytes; limit=$preservingLimit"
    )
    assert(
      promotingMedian <= promotingLimit,
      s"Byte-to-Float Gaussian allocated $promotingMedian bytes; limit=$promotingLimit"
    )
    assert(
      preparedMedian <= preparedLimit,
      s"prepared Float Gaussian allocated $preparedMedian bytes; limit=$preparedLimit"
    )
    println(
      s"IMG-FILTER JVM allocation: operation=gaussian-preserving-float, " +
        s"samples=$samples, allocated=$preservingMedian B"
    )
    println(
      s"IMG-FILTER JVM allocation: operation=gaussian-byte-to-float, " +
        s"samples=$samples, allocated=$promotingMedian B"
    )
    println(
      s"IMG-FILTER JVM allocation: operation=gaussian-prepared-float, " +
        s"samples=$samples, allocated=$preparedMedian B"
    )
    assert(retained != null)

  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean
            if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then
            value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("thread allocation accounting is unavailable")
    val thread = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(thread)
    body
    bean.getThreadAllocatedBytes(thread) - before

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
