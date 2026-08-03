package image4s.ops

import scala.compiletime.testing.typeCheckErrors

import image4s.Continuous
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite

final class OpsCoreVocabSuite extends FunSuite:
  test("Offset and Support are finite, ordered, and duplicate-free"):
    val a = opsRight(Offset.create[D2](Vector(1, 0)))
    val b = opsRight(Offset.create[D2](Vector(0, 1)))
    val c = opsRight(Offset.create[D2](Vector(-1, 0)))
    assert(Support.create(Vector(a, b, c, a)).isLeft)
    val unique = opsRight(Support.create(Vector(b, a, c)))
    assertEquals(unique.size, 3)
    assertEquals(unique.offsets(0).coordinates.toSeq, Seq(-1, 0))
    assertEquals(unique.leftExtents.toSeq, Seq(1, 0))
    assertEquals(unique.rightExtents.toSeq, Seq(1, 1))

  test("AxisKernel requires an explicit anchor for even lengths"):
    val odd = opsRight(AxisKernel.centered(Vector(1.0, 2.0, 1.0)))
    assertEquals(odd.anchor, 1)
    assert(AxisKernel.centered(Vector(1.0, 2.0)).isLeft)
    val even = opsRight(AxisKernel.create(Vector(1.0, 2.0, 3.0, 4.0), 1))
    assertEquals(even.anchor, 1)

  test("Kernel dense/sparse/separable and named corr vs conv"):
    val support =
      opsRight(
        Support.create(
          Vector(
            opsRight(Offset.create[D2](Vector(0, 0))),
            opsRight(Offset.create[D2](Vector(1, 0)))
          )
        )
      )
    val dense =
      opsRight(Kernel.dense(support, Vector(1.0, 0.5)))
    val sparse =
      opsRight(Kernel.sparse(support, Vector(1.0, 0.5)))
    val sep =
      opsRight(
        Kernel.separable[D2, Double](
          Vector(
            opsRight(AxisKernel.centered(Vector(0.25, 0.5, 0.25))),
            opsRight(AxisKernel.centered(Vector(0.25, 0.5, 0.25)))
          )
        )
      )
    val corr = Correlation(dense, FilterExtent.same(Border.reflect))
    val conv = Convolution(sparse, FilterExtent.valid)
    assertEquals(corr.op, LinearNeighborhoodOp.Correlation)
    assertEquals(conv.op, LinearNeighborhoodOp.Convolution)
    assertEquals(sep.support.size, 9)

  test("FilterExtent Valid cannot carry a border"):
    val same: FilterExtent[Double] = FilterExtent.same(Border.Constant(0.0))
    val valid: FilterExtent[Nothing] = FilterExtent.valid
    val full: FilterExtent[Double] = FilterExtent.full(Border.Replicate)
    same match
      case FilterExtent.Same(_) => ()
      case _ => fail("expected Same")
    valid match
      case FilterExtent.Valid => ()
      case _ => fail("expected Valid")
    full match
      case FilterExtent.Full(_) => ()
      case _ => fail("expected Full")

  test("SpatialSigma and Radius distinguish samples vs frame"):
    val sample = opsRight(SpatialSigma.samples[D3](1.5))
    val frame = opsRight(SpatialSigma.frame[D2](2.0))
    val radius = opsRight(Radius.frame(1.0))
    assert(sample.isInstanceOf[SpatialSigma.Samples[?]])
    assert(frame.isInstanceOf[SpatialSigma.FrameUnits[?]])
    assert(radius.isInstanceOf[Radius.FrameUnits])
    assert(SpatialSigma.samples[D2](-1.0).isLeft)
    assert(Radius.samples(-1).isLeft)

  test("SupportsLinearFiltering admits Continuous and rejects Label/Mask"):
    summon[SupportsLinearFiltering[Continuous]]
    val categoricalErrors = typeCheckErrors(
      "import image4s.ops.*; import image4s.Categorical; summon[SupportsLinearFiltering[Categorical]]"
    )
    val maskErrors = typeCheckErrors(
      "import image4s.ops.*; import image4s.Mask; summon[SupportsLinearFiltering[Mask]]"
    )
    assert(categoricalErrors.nonEmpty)
    assert(maskErrors.nonEmpty)

  test("Same/Valid/Full output-grid laws"):
    val frame = geometryRight(Frame.named[D2]("ops-grid"))
    val source =
      geometryRight(
        Grid.in(frame)(
          Vector(5, 4),
          geometryRight(
            Affine.fromOriginSpacingDirection[D2](
              origin = Vector(10.0, 20.0),
              spacing = Vector(2.0, 3.0),
              directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
            )
          )
        )
      )
    val support =
      opsRight(
        Support.create(
          Vector(
            opsRight(Offset.create[D2](Vector(-1, -1))),
            opsRight(Offset.create[D2](Vector(0, 0))),
            opsRight(Offset.create[D2](Vector(1, 1)))
          )
        )
      )
    val same =
      opsRight(
        OutputGrid.grid(source, support, FilterExtent.same(Border.reflect))
      )
    assert(same eq source)

    val valid =
      opsRight(OutputGrid.grid(source, support, FilterExtent.valid))
    assertEquals(valid.shape, Vector(3, 2))
    assertEquals(
      geometryRight(valid.indexToFrame.apply(Vector(0.0, 0.0))),
      geometryRight(source.indexToFrame.apply(Vector(1.0, 1.0)))
    )

    val full =
      opsRight(
        OutputGrid.grid(
          source,
          support,
          FilterExtent.full(Border.Replicate)
        )
      )
    assertEquals(full.shape, Vector(7, 6))
    assertEquals(
      geometryRight(full.indexToFrame.apply(Vector(1.0, 1.0))),
      geometryRight(source.indexToFrame.apply(Vector(0.0, 0.0)))
    )

  test("ExecutionPolicy and plan contracts are schedule, not meaning"):
    val policy = ExecutionPolicy(method = FilterMethod.Separable)
    assertEquals(policy.method, FilterMethod.Separable)
    val workspace = Workspace.allocate(128)
    assertEquals(workspace.size, 128)
    val report =
      PlanReport(
        method = SelectedMethod.Separable,
        passes = 2,
        inputMaterialized = false,
        outputShape = Vector(8, 8),
        workspaceBytes = 128
      )
    assertEquals(report.passes, 2)

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
