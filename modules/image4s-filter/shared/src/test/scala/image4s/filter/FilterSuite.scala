package image4s.filter

import image4s.Axis
import image4s.AxisKind
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.ops.Border
import image4s.ops.Convolution
import image4s.ops.Correlation
import image4s.ops.ExecutionPolicy
import image4s.ops.FilterExtent
import image4s.ops.FilterMethod
import image4s.ops.Kernel
import image4s.ops.Offset
import image4s.ops.OpError
import image4s.ops.SelectedMethod
import image4s.ops.SpatialSigma
import image4s.ops.Support
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import scala.compiletime.testing.typeCheckErrors

final class FilterSuite extends FunSuite:
  test("Float Gaussian preserves dtype and exact Same grid"):
    val image =
      floatImage(
        Vector.tabulate(81)(index => if index == 40 then 1.0f else 0.0f),
        Vector(9, 9)
      )
    val sigma = opsRight(SpatialSigma.samples[D2](1.0))
    val output = opsRight(image.gaussianBlur(sigma))

    assert(output.grid.sameRuntimeOwnerAs(image.grid))
    assertEquals(output.dtype.name, "Float")
    assertEquals(output.logicalShape, image.logicalShape)
    assertEqualsDouble(
      output.data.elementsIterator.map(_.toDouble).sum,
      1.0,
      1.0e-6
    )

  test("Byte Gaussian requires explicit promotion and preserves batch axes"):
    val frame = geometryRight(Frame.named[D2]("byte-batch"))
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(5, 5),
          Affine.identity[D2]
        )
      )
    val channels =
      axisRight(Axis.ordinal("channel", AxisKind.Channel, 2))
    val space =
      SampleSpace.create(grid, nonSpatialRight(NonSpatialAxes.from(Vector(channels))))
    val data =
      NDArray.tabulate[Byte](5, 5, 2) { (_, _, channel) =>
        if channel == 0 then 10.toByte else 100.toByte
      }
    val image =
      imageRight(Sampled.continuous[Byte, Rank[3]](space, data))
    val sigma = opsRight(SpatialSigma.samples[D2](1.0))
    val output =
      opsRight(
        image.gaussianBlurTo[Float](
          sigma,
          FilterExtent.same(Border.Replicate)
        )
      )

    assertEquals(output.logicalShape, Vector(5, 5, 2))
    var row = 0
    while row < 5 do
      var column = 0
      while column < 5 do
        assertEqualsDouble(output.data(row, column, 0).toDouble, 10.0, 1.0e-5)
        assertEqualsDouble(output.data(row, column, 1).toDouble, 100.0, 1.0e-4)
        column += 1
      row += 1

    assert(
      typeCheckErrors(
        "import image4s.filter.*; summon[FilterOutput[Byte]]"
      ).nonEmpty
    )

  test("convolution equals correlation with exact kernel reversal"):
    val image =
      doubleImage(
        Vector.tabulate(25)(_.toDouble),
        Vector(5, 5)
      )
    val support =
      opsRight(
        Support.create[D2](
          Vector(
            Offset.unsafe[D2](Vector(-1, 0)),
            Offset.unsafe[D2](Vector(0, 0)),
            Offset.unsafe[D2](Vector(1, 0))
          )
        )
      )
    val kernel =
      opsRight(Kernel.sparse(support, Vector(1.0, 2.0, 3.0)))
    val reversed =
      opsRight(Kernel.sparse(support, Vector(3.0, 2.0, 1.0)))
    val extent = FilterExtent.same(Border.Constant(0.0))

    val convolved =
      opsRight(
        LinearFilter.convolve(
          image,
          Convolution[D2, Double](kernel, extent)
        )
      )
    val correlated =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation[D2, Double](reversed, extent)
        )
      )

    assertEquals(
      convolved.data.elementsIterator.toVector,
      correlated.data.elementsIterator.toVector
    )

  test("dense, sparse, and separable correlation agree"):
    val image =
      doubleImage(
        Vector.tabulate(49)(index => (index % 11).toDouble),
        Vector(7, 7)
      )
    val horizontal =
      opsRight(image4s.ops.AxisKernel.centered(Vector(0.25, 0.5, 0.25)))
    val vertical =
      opsRight(image4s.ops.AxisKernel.centered(Vector(0.2, 0.6, 0.2)))
    val separable =
      opsRight(Kernel.separable[D2, Double](Vector(horizontal, vertical)))
    val denseWeights =
      separable.support.offsets.map { offset =>
        horizontal.weights(offset.coordinates(0) + horizontal.anchor) *
          vertical.weights(offset.coordinates(1) + vertical.anchor)
      }
    val dense =
      opsRight(Kernel.dense(separable.support, denseWeights))
    val sparse =
      opsRight(Kernel.sparse(separable.support, denseWeights))
    val extent = FilterExtent.same(Border.ReflectWithEdge)
    val outputs =
      Vector(
        (dense, FilterMethod.Direct),
        (sparse, FilterMethod.Direct),
        (separable, FilterMethod.Separable)
      ).map { case (kernel, method) =>
        opsRight(
          LinearFilter.correlate(
            image,
            Correlation[D2, Double](kernel, extent),
            ExecutionPolicy(method = method)
          )
        ).data.elementsIterator.toVector
      }

    assertEquals(outputs(0), outputs(1))
    outputs(0).zip(outputs(2)).foreach { case (expected, actual) =>
      assertEqualsDouble(actual, expected, 1.0e-12)
    }

  test("Double Gaussian supports D3"):
    val frame = geometryRight(Frame.named[D3]("d3-filter"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(7, 7, 7), Affine.identity[D3])
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val data =
      NDArray.tabulate[Double](7, 7, 7) { (x, y, z) =>
        if x == 3 && y == 3 && z == 3 then 1.0 else 0.0
      }
    val image =
      imageRight(Sampled.continuous[Double, Rank[3]](space, data))
    val sigma = opsRight(SpatialSigma.samples[D3](1.0))
    val plan =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          FilterExtent.same(Border.Constant(0.0))
        )
      )
    val output =
      opsRight(
        plan.run(
          image
        )
      )

    assertEquals(plan.report.method, SelectedMethod.Separable)
    assertEquals(plan.report.passes, 3)
    assertEqualsDouble(output.data.elementsIterator.sum, 1.0, 1.0e-12)

  test("frame-space Gaussian converts axis-aligned physical scales"):
    val frame = geometryRight(Frame.named[D2]("physical-filter"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(0.0, 0.0),
          spacing = Vector(2.0, 4.0),
          directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
        )
      )
    val grid = geometryRight(Grid.in(frame)(Vector(9, 9), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val data =
      NDArray.tabulate[Double](9, 9) { (row, column) =>
        if row == 4 && column == 4 then 1.0 else 0.0
      }
    val image =
      imageRight(Sampled.continuous[Double, Rank[2]](space, data))
    val frameSigma =
      opsRight(SpatialSigma.frame[D2](Vector(2.0, 4.0), None))
    val sampleSigma =
      opsRight(SpatialSigma.samples[D2](Vector(1.0, 1.0)))
    val physical = opsRight(image.gaussianBlur(frameSigma))
    val sample = opsRight(image.gaussianBlur(sampleSigma))

    assertEquals(
      physical.data.elementsIterator.toVector,
      sample.data.elementsIterator.toVector
    )

  test("prepared Gaussian reuses its schedule without aliasing prior results"):
    val image =
      floatImage(
        Vector.tabulate(81)(index => if index == 40 then 1.0f else 0.0f),
        Vector(9, 9)
      )
    val sigma = opsRight(SpatialSigma.samples[D2](1.0))
    val plan =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          policy = ExecutionPolicy(method = FilterMethod.Direct)
        )
      )
    val first = opsRight(plan.run(image))
    val firstValues = first.data.elementsIterator.toVector
    val next =
      imageRight(
        Sampled.continuous[Float, Rank[2]](
          image.sampleSpace,
          NDArray.tabulate[Float](9, 9)((_, _) => 2.0f)
        )
      )
    val second = opsRight(plan.run(next))

    assertEquals(first.data.elementsIterator.toVector, firstValues)
    assert(
      second.data.elementsIterator.forall(value =>
        math.abs(value - 2.0f) < 1.0e-5f
      ),
      "prepared run did not replace every workspace value"
    )
    assertEquals(plan.report.passes, 1)
    assert(plan.report.workspaceBytes > 0L)

  test("separable Gaussian uses reusable ping-pong workspaces"):
    val image =
      doubleImage(
        Vector.tabulate(81)(index => (index % 13).toDouble),
        Vector(9, 9)
      )
    val sigma = opsRight(SpatialSigma.samples[D2](Vector(1.0, 1.5)))
    val direct =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          policy = ExecutionPolicy(method = FilterMethod.Direct)
        )
      )
    val separable =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val directOutput = opsRight(direct.run(image))
    val separableOutput = opsRight(separable.run(image))

    assertEquals(separable.report.method, SelectedMethod.Separable)
    assertEquals(separable.report.passes, 2)
    assertEquals(
      separable.report.workspaceBytes,
      direct.report.workspaceBytes * 2L
    )
    directOutput.data.elementsIterator
      .zip(separableOutput.data.elementsIterator)
      .foreach { case (expected, actual) =>
        assertEqualsDouble(actual, expected, 1.0e-10)
      }

  test("separable filtering supports strided inputs"):
    val canonical =
      doubleImage(
        Vector.tabulate(81)(index => (index % 17).toDouble),
        Vector(9, 9)
      )
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[2]](
          canonical.sampleSpace,
          canonical.data.reverse(0)
        )
      )
    val sigma = opsRight(SpatialSigma.samples[D2](Vector(1.0, 1.5)))
    val direct =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          policy = ExecutionPolicy(method = FilterMethod.Direct)
        )
      )
    val separable =
      opsRight(
        Gaussian.prepare(
          image,
          sigma,
          policy = ExecutionPolicy(method = FilterMethod.Separable)
        )
      )
    val directOutput = opsRight(direct.run(image))
    val separableOutput = opsRight(separable.run(image))

    directOutput.data.elementsIterator
      .zip(separableOutput.data.elementsIterator)
      .foreach { case (expected, actual) =>
        assertEqualsDouble(actual, expected, 1.0e-10)
      }

  test("anisotropic frame Gaussian rejects a non-separable sheared grid"):
    val frame = geometryRight(Frame.named[D2]("sheared"))
    val affine =
      geometryRight(
        Affine.fromRowMajor[D2](
          Vector(
            1.0,
            0.5,
            0.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            1.0
          )
        )
      )
    val grid = geometryRight(Grid.in(frame)(Vector(5, 5), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[2]](
          space,
          NDArray.tabulate[Double](5, 5)((_, _) => 1.0)
        )
      )
    val sigma =
      opsRight(SpatialSigma.frame[D2](Vector(1.0, 2.0), None))

    assert(image.gaussianBlur(sigma).isLeft)

  private def floatImage(
      values: Vector[Float],
      shape: Vector[Int]
  ) =
    val frame = geometryRight(Frame.named[D2]("float-filter"))
    val grid =
      geometryRight(
        Grid.in(frame)(
          shape,
          Affine.identity[D2]
        )
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Float, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def doubleImage(
      values: Vector[Double],
      shape: Vector[Int]
  ) =
    val frame = geometryRight(Frame.named[D2]("double-filter"))
    val grid =
      geometryRight(
        Grid.in(frame)(
          shape,
          Affine.identity[D2]
        )
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def axisRight(value: Either[image4s.ImageError, Axis]): Axis =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def nonSpatialRight(
      value: Either[image4s.ImageError, NonSpatialAxes]
  ): NonSpatialAxes =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
