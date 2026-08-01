package image4s.locus

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LengthUnit
import locus4s.DomainRegistry
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class GridDomainAllocationSuite extends FunSuite:
  @volatile private var sink: Long = 0L

  test("spatial field construction allocates metadata, never a voxel buffer"):
    val frame =
      right(
        Frame.persistentNamed[D3](
          right(FrameId.parse("frame-field-allocation")),
          "allocation",
          LengthUnit.Millimeter,
          CoordinateConvention.RAS
        )
      )
    val grid =
      right(
        Grid.createPersistent(
          right(GridId.parse("grid-field-allocation")),
          frame
        )(
          Vector(96, 96, 60),
          Affine.identity[D3]
        )
      )
    val bridge =
      right(
        GridDomain.register(
          grid,
          "voxels",
          DomainRegistry.empty
        )
      ).value
    val image =
      right(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](96, 96, 60)
        )
      )

    var warmup = 0
    while warmup < 100 do
      consume(right(bridge.spatialField(image)))
      warmup += 1

    val samples =
      Vector.fill(21) {
        allocatedBytes {
          consume(right(bridge.spatialField(image)))
        }
      }.sorted
    val median = samples(samples.size / 2)
    val field = right(bridge.spatialField(image))

    assert(field.sourceData eq image.data)
    assert(
      median <= 64L * 1024L,
      s"spatial field construction allocated $median bytes for " +
        s"${image.data.size} voxels"
    )
    println(
      s"IMG-LOCUS-FIELD allocation: voxels=${image.data.size}, " +
        s"allocated=$median B"
    )

  private def consume(value: AnyRef): Unit =
    sink = sink + System.identityHashCode(value).toLong

  private def allocatedBytes(body: => Unit): Long =
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
    bean.getThreadAllocatedBytes(threadId) - before

  private def right[E, A](value: Either[E, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(s"expected Right, found Left($error)")
