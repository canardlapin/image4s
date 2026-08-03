package image4s.ops.laws

import java.lang.management.ManagementFactory

import com.sun.management.ThreadMXBean
import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.filter.Gaussian
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.morphology.BinaryMorphology
import image4s.morphology.StructuringElement
import image4s.ops.ExecutionPolicy
import image4s.ops.FilterMethod
import image4s.ops.OpError
import image4s.ops.Radius
import image4s.ops.SpatialSigma
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

final class OpsPerformanceCourtSuite extends FunSuite:
  private var retained: AnyRef = null

  test("prepared direct and separable filters allocate only their result"):
    val edge = 128
    val samples2D = edge * edge
    val canonicalFloat = float2DImage(edge)
    val stridedDouble = double2DImage(edge)
    val stridedFloat3D = float3DImage(32)
    val batchedFloat = batchedFloat2DImage(edge, batches = 2)
    val mask2D = mask2DImage(edge)

    val directFloat =
      opsRight(
        Gaussian.prepare(
          canonicalFloat,
          opsRight(SpatialSigma.samples[D2](1.25)),
          policy = ExecutionPolicy(method = FilterMethod.Direct)
        )
      )
    val directFloatLarge =
      opsRight(
        Gaussian.prepare(
          canonicalFloat,
          opsRight(SpatialSigma.samples[D2](4.0)),
          policy = ExecutionPolicy(method = FilterMethod.Direct)
        )
      )
    val separableDouble =
      opsRight(
        Gaussian.prepare(
          stridedDouble,
          opsRight(SpatialSigma.samples[D2](1.25)),
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val separableDoubleLarge =
      opsRight(
        Gaussian.prepare(
          stridedDouble,
          opsRight(SpatialSigma.samples[D2](4.0)),
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val separableFloat3D =
      opsRight(
        Gaussian.prepare(
          stridedFloat3D,
          opsRight(SpatialSigma.samples[D3](1.25)),
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val separableBatchedFloat =
      opsRight(
        Gaussian.prepare(
          batchedFloat,
          opsRight(SpatialSigma.samples[D2](1.25)),
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val preparedDilate =
      opsRight(
        BinaryMorphology.prepareDilate(
          mask2D,
          StructuringElement.cross[D2](opsRight(Radius.samples(1)))
        )
      )
    val cases =
      Vector(
        CourtCase(
          "D2-canonical-direct-float",
          samples2D,
          bytesPerSample = 4,
          () => retained = opsRight(directFloat.run(canonicalFloat)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D2-canonical-direct-float-large-kernel",
          samples2D,
          bytesPerSample = 4,
          () => retained = opsRight(directFloatLarge.run(canonicalFloat)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D2-strided-separable-double",
          samples2D,
          bytesPerSample = 8,
          () => retained = opsRight(separableDouble.run(stridedDouble)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D2-strided-separable-double-large-kernel",
          samples2D,
          bytesPerSample = 8,
          () => retained = opsRight(separableDoubleLarge.run(stridedDouble)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D2-batched-separable-float",
          samples2D * 2,
          bytesPerSample = 4,
          () => retained = opsRight(separableBatchedFloat.run(batchedFloat)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D3-strided-separable-float",
          32 * 32 * 32,
          bytesPerSample = 4,
          () => retained = opsRight(separableFloat3D.run(stridedFloat3D)).asInstanceOf[AnyRef]
        ),
        CourtCase(
          "D2-canonical-morphology-dilate",
          samples2D,
          bytesPerSample = 1,
          () => retained = opsRight(preparedDilate.run(mask2D)).asInstanceOf[AnyRef]
        )
      )

    cases.foreach { courtCase =>
      Vector.fill(4)(courtCase.run())
      val allocated =
        Vector.fill(7)(allocatedBytes(courtCase.run())).sorted.apply(3)
      val limit =
        courtCase.samples.toLong * courtCase.bytesPerSample.toLong +
          128L * 1024L
      assert(
        allocated <= limit,
        s"${courtCase.name} allocated $allocated bytes; limit=$limit"
      )
      println(
        s"IMG-OPS JVM allocation: case=${courtCase.name}, " +
          s"samples=${courtCase.samples}, allocated=$allocated B"
      )
    }
    assert(retained != null)

  private final case class CourtCase(
      name: String,
      samples: Int,
      bytesPerSample: Int,
      run: () => Unit
  )

  private def float2DImage(edge: Int) =
    val frame = geometryRight(Frame.named[D2]("ops-court-float"))
    val grid = geometryRight(Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Float, Rank[2]](
        space,
        NDArray.tabulate[Float](edge, edge)((row, column) => ((row + column) % 23).toFloat)
      )
    )

  private def double2DImage(edge: Int) =
    val frame = geometryRight(Frame.named[D2]("ops-court-double"))
    val grid = geometryRight(Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray
          .tabulate[Double](edge, edge)((row, column) => ((row * 3 + column) % 29).toDouble)
          .reverse(0)
      )
    )

  private def batchedFloat2DImage(edge: Int, batches: Int) =
    val frame = geometryRight(Frame.named[D2]("ops-court-batched"))
    val grid = geometryRight(Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val channel = imageRight(Axis.ordinal("channel", AxisKind.Channel, batches))
    val axes = imageRight(NonSpatialAxes.from(Vector(channel)))
    val space = SampleSpace.create(grid, axes)
    imageRight(
      Sampled.continuous[Float, Rank[3]](
        space,
        NDArray.tabulate[Float](edge, edge, batches)((row, column, batch) =>
          ((row + column + batch * 7) % 23).toFloat
        )
      )
    )

  private def mask2DImage(edge: Int) =
    val frame = geometryRight(Frame.named[D2]("ops-court-mask"))
    val grid = geometryRight(Grid.in(frame)(Vector(edge, edge), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.mask(
        space,
        NDArray.tabulate[Boolean](edge, edge)((row, column) => (row + 3 * column) % 11 == 0)
      )
    )

  private def float3DImage(edge: Int) =
    val frame = geometryRight(Frame.named[D3]("ops-court-d3"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(edge, edge, edge), Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Float, Rank[3]](
        space,
        NDArray
          .tabulate[Float](edge, edge, edge)((x, y, z) => ((x + 2 * y + 3 * z) % 31).toFloat)
          .reverse(0)
      )
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
