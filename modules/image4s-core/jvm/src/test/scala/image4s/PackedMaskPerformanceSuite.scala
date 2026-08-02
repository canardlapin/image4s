package image4s

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.packed.PackedArray
import ravel.packed.PackedBits

final class PackedMaskPerformanceSuite extends FunSuite:
  private var retained: AnyRef = null

  test("wordwise packed mask union never expands to Booleans"):
    val edge = 1024
    val samples = edge * edge
    val frame = geometryRight(Frame.named[D2]("packed-mask-court"))
    val grid =
      geometryRight(image4s.geometry.Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      packedRight(
        PackedSampled.packMask(
          imageRight(
            Sampled.mask(
              space,
              NDArray.tabulate[Boolean](edge, edge)((row, column) =>
                (row * 5 + column) % 7 < 3
              )
            )
          )
        )
      )
    val right =
      packedRight(
        PackedSampled.packMask(
          imageRight(
            Sampled.mask(
              space,
              NDArray.tabulate[Boolean](edge, edge)((row, column) =>
                (row + column * 3) % 5 < 2
              )
            )
          )
        )
      )
    val run = () =>
      retained = packedRight(left.union(right)).asInstanceOf[AnyRef]

    Vector.fill(5)(run())
    val allocated = Vector.fill(7)(allocatedBytes(run())).sorted.apply(3)
    val wordBytes = PackedArray.wordCount(samples, PackedBits.B1).toLong * 4L
    val booleanBytes = samples.toLong

    assert(
      allocated <= wordBytes + 8192L,
      s"wordwise union allocated $allocated B; words need $wordBytes B and " +
        s"a Boolean expansion would need at least $booleanBytes B"
    )
    println(
      s"IMG-PACKED JVM allocation: case=mask-union, samples=$samples, " +
        s"allocated=$allocated B, booleanExpansion=$booleanBytes B"
    )

  test("fused packed-to-Float decode allocates only the Float output"):
    val edge = 512
    val samples = edge * edge
    val frame = geometryRight(Frame.named[D2]("packed-decode-court"))
    val grid =
      geometryRight(image4s.geometry.Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val quantizer =
      packedRight(
        PackedEncoding.UniformQuantizer.create(0.0, 1.0, PackedBits.B4)
      )
    val packed =
      packedRight(
        PackedSampled.pack(
          imageRight(
            Sampled.continuous[Double, ravel.Rank[2]](
              space,
              NDArray.tabulate[Double](edge, edge)((row, column) =>
                ((row + column) % 16).toDouble / 15.0
              )
            )
          ),
          quantizer
        )
      )
    val run = () =>
      retained = packed.decodeToFloatArray

    Vector.fill(5)(run())
    val allocated = Vector.fill(7)(allocatedBytes(run())).sorted.apply(3)
    val outputBytes = samples.toLong * 4L

    assert(
      allocated <= outputBytes + 8192L,
      s"fused decode allocated $allocated B; output needs $outputBytes B"
    )
    println(
      s"IMG-PACKED JVM allocation: case=fused-decode-float, samples=$samples, " +
        s"allocated=$allocated B, output=$outputBytes B"
    )
    assert(retained != null)

  private def allocatedBytes(body: => Unit): Long =
    val bean =
      ManagementFactory.getThreadMXBean match
        case value: ThreadMXBean if value.isThreadAllocatedMemorySupported =>
          if !value.isThreadAllocatedMemoryEnabled then
            value.setThreadAllocatedMemoryEnabled(true)
          value
        case _ =>
          fail("thread allocation accounting is unavailable")
    val thread = Thread.currentThread().threadId()
    val before = bean.getThreadAllocatedBytes(thread)
    body
    bean.getThreadAllocatedBytes(thread) - before

  private def packedRight[A](value: Either[PackedImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
