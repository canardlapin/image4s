package image4s

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.ConversionPolicy
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

final class ConversionPerformanceSuite extends FunSuite:
  private var retained: AnyRef = null

  test("Byte and Short to Float conversions have primitive allocation receipts"):
    val edge = 512
    val samples = edge * edge
    val frame = geometryRight(Frame.named[D2]("conversion-performance"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(edge, edge), Affine.identity[D2])
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val bytes =
      imageRight(
        Sampled.continuous[Byte, Rank[2]](
          space,
          NDArray.tabulate[Byte](edge, edge) { (row, column) =>
            ((row + column) % 127).toByte
          }
        )
      )
    val shorts =
      imageRight(
        Sampled.continuous[Short, Rank[2]](
          space,
          NDArray.tabulate[Short](edge, edge) { (row, column) =>
            ((row * 31 + column) % 32767).toShort
          }
        )
      )
    val byteRun = () =>
      retained = imageRight(bytes.convertTo[Float](ConversionPolicy()))
        .asInstanceOf[AnyRef]
    val shortRun = () =>
      retained = imageRight(shorts.convertTo[Float](ConversionPolicy()))
        .asInstanceOf[AnyRef]

    Vector.fill(3) {
      byteRun()
      shortRun()
    }
    check("byte-to-float", samples, byteRun())
    check("short-to-float", samples, shortRun())
    assert(retained != null)

  private def check(
      name: String,
      samples: Int,
      body: => Unit
  ): Unit =
    val allocations =
      Vector.fill(7)(allocatedBytes(body)).sorted
    val timings =
      Vector
        .fill(11) {
          val started = System.nanoTime()
          body
          System.nanoTime() - started
        }
        .sorted
    val allocated = allocations(allocations.size / 2)
    val elapsed = timings(timings.size / 2)
    val outputBytes = samples.toLong * 4L
    val limit = outputBytes + 128L * 1024L
    val samplesPerSecond =
      samples.toDouble * 1.0e9 / elapsed.toDouble

    assert(
      allocated <= limit,
      s"$name allocated $allocated bytes; Float output requires $outputBytes"
    )
    println(
      f"IMG-CONVERSION JVM baseline: operation=$name, samples=$samples%d, " +
        f"allocated=$allocated%d B, median=$elapsed%d ns, " +
        f"millionSamplesPerSecond=${samplesPerSecond / 1.0e6}%.3f"
    )

  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("thread allocation accounting is unavailable")
    val thread = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(thread)
    body
    bean.getThreadAllocatedBytes(thread) - before

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
