package image4s.ops.laws

import image4s.Axis
import image4s.AxisKind
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.filter.Gaussian
import image4s.filter.Gradient
import image4s.filter.LinearFilter
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Frame
import image4s.morphology.BinaryMorphology
import image4s.morphology.StructuringElement
import image4s.ops.AxisKernel
import image4s.ops.Convolution
import image4s.ops.Border
import image4s.ops.Correlation
import image4s.ops.ExecutionPolicy
import image4s.ops.FilterExtent
import image4s.ops.FilterMethod
import image4s.ops.Kernel
import image4s.ops.Offset
import image4s.ops.OpError
import image4s.ops.Radius
import image4s.ops.SpatialSigma
import image4s.ops.Support
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

final class OpsConformanceSuite extends FunSuite:
  test("filter output-grid laws hold for Same, Valid, and Full"):
    val image =
      doubleImage(
        Vector.tabulate(35)(_.toDouble),
        Vector(7, 5)
      )
    val kernel = horizontalKernel
    val same =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(kernel, FilterExtent.same(Border.Constant(0.0)))
        )
      )
    val valid =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(kernel, FilterExtent.valid)
        )
      )
    val full =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(kernel, FilterExtent.full(Border.Constant(0.0)))
        )
      )

    assertEquals(same.logicalShape, Vector(7, 5))
    assert(same.grid.sameRuntimeOwnerAs(image.grid))
    assertEquals(valid.logicalShape, Vector(5, 5))
    assertEquals(valid.grid.indexToFrame.rowMajor(2), 1.0)
    assertEquals(full.logicalShape, Vector(9, 5))
    assertEquals(full.grid.indexToFrame.rowMajor(2), -1.0)

  test("direct and separable filtering agree for strided batched input"):
    val frame = geometryRight(Frame.named[D2]("ops-law-batch"))
    val grid = geometryRight(Grid.in(frame)(Vector(7, 7), Affine.identity[D2]))
    val channel = imageRight(Axis.ordinal("channel", AxisKind.Channel, 2))
    val axes = imageRight(NonSpatialAxes.from(Vector(channel)))
    val space = SampleSpace.create(grid, axes)
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[3]](
          space,
          NDArray
            .tabulate[Double](7, 7, 2)((row, column, batch) =>
              (row * 7 + column + batch * 100).toDouble
            )
            .reverse(0)
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
    val expected = opsRight(direct.run(image))
    val actual = opsRight(separable.run(image))

    expected.data.elementsIterator
      .zip(actual.data.elementsIterator)
      .foreach { case (left, right) =>
        assertEqualsDouble(right, left, 1.0e-10)
      }

  test("prepared filters return immutable non-aliasing results"):
    val first =
      doubleImage(
        Vector.tabulate(49)(index => if index == 24 then 1.0 else 0.0),
        Vector(7, 7)
      )
    val second =
      imageRight(
        Sampled.continuous[Double, Rank[2]](
          first.sampleSpace,
          NDArray.fromSeq(Shape(7, 7), Vector.fill(49)(2.0))
        )
      )
    val sigma = opsRight(SpatialSigma.samples[D2](1.0))
    val plan = opsRight(Gaussian.prepare(first, sigma))
    val result = opsRight(plan.run(first))
    val before = result.data.elementsIterator.toVector
    val next = opsRight(plan.run(second))

    assertEquals(result.data.elementsIterator.toVector, before)
    assert(next.data.elementsIterator.forall(value => math.abs(value - 2.0) < 1.0e-12))

  test("morphology is monotone and opening is idempotent"):
    val lower =
      maskImage(
        Vector(
          false, false, false, false, false,
          false, true, false, false, false,
          false, false, false, false, false,
          false, false, false, false, false,
          false, false, false, false, false
        )
      )
    val upper =
      maskImage(
        Vector(
          false, false, false, false, false,
          false, true, true, false, false,
          false, false, true, false, false,
          false, false, false, false, false,
          false, false, false, false, false
        )
      )
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val lowerDilated = opsRight(BinaryMorphology.dilate(lower, element))
    val upperDilated = opsRight(BinaryMorphology.dilate(upper, element))
    val opened = opsRight(BinaryMorphology.open(upper, element))
    val openedAgain = opsRight(BinaryMorphology.open(opened, element))

    lowerDilated.data.elementsIterator
      .zip(upperDilated.data.elementsIterator)
      .foreach { case (left, right) =>
        assert(!left || right, "dilation must preserve subset ordering")
      }
    assertEquals(
      openedAgain.data.elementsIterator.toVector,
      opened.data.elementsIterator.toVector
    )

  test("filtering is layout independent"):
    val canonical =
      doubleImage(
        Vector.tabulate(49)(index => ((index * 13) % 17).toDouble),
        Vector(7, 7)
      )
    val strided =
      imageRight(
        Sampled.continuous[Double, Rank[2]](
          canonical.sampleSpace,
          NDArray
            .tabulate[Double](7, 7)((row, column) =>
              (((6 - row) * 7 + column) * 13 % 17).toDouble
            )
            .reverse(0)
        )
      )
    canonical.data.elementsIterator
      .zip(strided.data.elementsIterator)
      .foreach { case (left, right) => assertEquals(left, right) }
    val operation =
      Correlation[D2, Double](
        horizontalKernel,
        FilterExtent.same(Border.Constant(0.0))
      )
    val fromCanonical = opsRight(LinearFilter.correlate(canonical, operation))
    val fromStrided = opsRight(LinearFilter.correlate(strided, operation))

    assertEquals(
      fromCanonical.data.elementsIterator.toVector,
      fromStrided.data.elementsIterator.toVector
    )

  test("batched filtering equals map over the stacked slices"):
    val frame = geometryRight(Frame.named[D2]("ops-law-map-stack"))
    val grid = geometryRight(Grid.in(frame)(Vector(6, 6), Affine.identity[D2]))
    val channel = imageRight(Axis.ordinal("channel", AxisKind.Channel, 3))
    val axes = imageRight(NonSpatialAxes.from(Vector(channel)))
    val space = SampleSpace.create(grid, axes)
    def sliceValue(row: Int, column: Int, batch: Int): Double =
      ((row * 6 + column) * (batch + 1) % 23).toDouble
    val batched =
      imageRight(
        Sampled.continuous[Double, Rank[3]](
          space,
          NDArray.tabulate[Double](6, 6, 3)(sliceValue)
        )
      )
    val sigma = opsRight(SpatialSigma.samples[D2](1.0))
    val blurred = opsRight(Gaussian.prepare(batched, sigma)).run(batched)
    val output = opsRight(blurred)

    var batch = 0
    while batch < 3 do
      val slice =
        doubleImage(
          Vector.tabulate(36)(index => sliceValue(index / 6, index % 6, batch)),
          Vector(6, 6)
        )
      val expected = opsRight(opsRight(Gaussian.prepare(slice, sigma)).run(slice))
      var row = 0
      while row < 6 do
        var column = 0
        while column < 6 do
          assertEqualsDouble(
            output.data(row, column, batch),
            expected.data(row, column),
            1.0e-12,
            s"batch $batch diverged at ($row, $column)"
          )
          column += 1
        row += 1
      batch += 1

  test("identity kernel returns the input values"):
    val image =
      doubleImage(
        Vector.tabulate(30)(index => ((index * 7) % 11).toDouble),
        Vector(6, 5)
      )
    val support =
      opsRight(Support.create[D2](Vector(Offset.unsafe[D2](Vector(0, 0)))))
    val identity = opsRight(Kernel.sparse(support, Vector(1.0)))
    val output =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(identity, FilterExtent.same(Border.Constant(0.0)))
        )
      )

    assertEquals(
      output.data.elementsIterator.toVector,
      image.data.elementsIterator.toVector
    )

  test("convolution equals correlation with the reflected kernel"):
    val image =
      doubleImage(
        Vector.tabulate(42)(index => ((index * 5) % 13).toDouble),
        Vector(7, 6)
      )
    val taps =
      Vector(
        Vector(-1, -1) -> 0.5,
        Vector(-1, 0) -> -1.0,
        Vector(0, 0) -> 2.0,
        Vector(1, 0) -> 3.0,
        Vector(0, 2) -> -0.25
      )
    val kernel = sparseKernel(taps)
    val reflected =
      sparseKernel(taps.map((offset, weight) => (offset.map(-_), weight)))
    val extent = FilterExtent.same(Border.Constant(0.0))
    val convolved =
      opsRight(LinearFilter.convolve(image, Convolution(kernel, extent)))
    val correlated =
      opsRight(LinearFilter.correlate(image, Correlation(reflected, extent)))

    assertEquals(
      convolved.data.elementsIterator.toVector,
      correlated.data.elementsIterator.toVector
    )

  test("dense, sparse, and separable encodings of one kernel agree"):
    val image =
      doubleImage(
        Vector.tabulate(64)(index => ((index * 11) % 19).toDouble),
        Vector(8, 8)
      )
    val rowWeights = Vector(0.25, 0.5, 0.25)
    val columnWeights = Vector(-0.5, 1.0, 0.5)
    val offsets =
      for
        rowOffset <- Vector(-1, 0, 1)
        columnOffset <- Vector(-1, 0, 1)
      yield Vector(rowOffset, columnOffset)
    val weights =
      offsets.map(offset =>
        rowWeights(offset(0) + 1) * columnWeights(offset(1) + 1)
      )
    val support =
      opsRight(Support.create[D2](offsets.map(Offset.unsafe[D2](_))))
    val dense = opsRight(Kernel.dense(support, weights))
    val sparse = opsRight(Kernel.sparse(support, weights))
    val separable =
      opsRight(
        Kernel.separable[D2, Double](
          Vector(
            opsRight(AxisKernel.centered(rowWeights)),
            opsRight(AxisKernel.centered(columnWeights))
          )
        )
      )
    val extent = FilterExtent.same(Border.Constant(0.0))
    val fromDense =
      opsRight(LinearFilter.correlate(image, Correlation(dense, extent)))
    val fromSparse =
      opsRight(LinearFilter.correlate(image, Correlation(sparse, extent)))
    val fromSeparable =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(separable, extent),
          ExecutionPolicy(method = FilterMethod.Separable)
        )
      )

    assertEquals(
      fromDense.data.elementsIterator.toVector,
      fromSparse.data.elementsIterator.toVector
    )
    fromDense.data.elementsIterator
      .zip(fromSeparable.data.elementsIterator)
      .foreach { case (left, right) =>
        assertEqualsDouble(right, left, 1.0e-12)
      }

  test("normalized filters preserve constant fields"):
    val constant =
      doubleImage(Vector.fill(49)(3.5), Vector(7, 7))
    val sigma = opsRight(SpatialSigma.samples[D2](1.25))
    val blurred =
      opsRight(
        opsRight(
          Gaussian.prepare(
            constant,
            sigma,
            extent = FilterExtent.same(Border.Replicate)
          )
        ).run(constant)
      )

    blurred.data.elementsIterator.foreach { value =>
      assertEqualsDouble(value, 3.5, 1.0e-12)
    }

  test("gradients of constant fields vanish"):
    val constant =
      doubleImage(Vector.fill(49)(2.25), Vector(7, 7))
    val field = opsRight(Gradient.sobel(constant))

    field.components.foreach { component =>
      component.data.elementsIterator.foreach { value =>
        assertEqualsDouble(value, 0.0, 1.0e-12)
      }
    }

  test("optimized correlation agrees with the naive reference oracle"):
    val shape = Vector(7, 6)
    val values = Vector.tabulate(42)(index => ((index * 17) % 23).toDouble)
    val image = doubleImage(values, shape)
    val offsets =
      Vector(
        Vector(-1, -1),
        Vector(-1, 1),
        Vector(0, 0),
        Vector(1, -2),
        Vector(2, 0)
      )
    val weights = Vector(0.5, -0.75, 2.0, 1.25, -0.125)
    val support =
      opsRight(Support.create[D2](offsets.map(Offset.unsafe[D2](_))))
    val kernel = opsRight(Kernel.sparse(support, weights))
    val output =
      opsRight(
        LinearFilter.correlate(
          image,
          Correlation(kernel, FilterExtent.same(Border.Constant(0.0)))
        )
      )
    def reference(row: Int, column: Int): Double =
      offsets.indices.foldLeft(0.0) { (acc, index) =>
        val sourceRow = row + offsets(index)(0)
        val sourceColumn = column + offsets(index)(1)
        val sample =
          if sourceRow < 0 || sourceRow >= shape(0) ||
            sourceColumn < 0 || sourceColumn >= shape(1)
          then 0.0
          else values(sourceRow * shape(1) + sourceColumn)
        acc + weights(index) * sample
      }

    var row = 0
    while row < shape(0) do
      var column = 0
      while column < shape(1) do
        assertEqualsDouble(
          output.data(row, column),
          reference(row, column),
          1.0e-12,
          s"correlation diverged from reference at ($row, $column)"
        )
        column += 1
      row += 1

  test("optimized dilation agrees with the naive reference oracle"):
    val values =
      Vector.tabulate(25)(index => (index * 7) % 13 < 4)
    val mask = maskImage(values)
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val output = opsRight(BinaryMorphology.dilate(mask, element))
    val offsets =
      Vector(
        Vector(-1, 0),
        Vector(0, -1),
        Vector(0, 0),
        Vector(0, 1),
        Vector(1, 0)
      )
    def reference(row: Int, column: Int): Boolean =
      offsets.exists { offset =>
        val sourceRow = row + offset(0)
        val sourceColumn = column + offset(1)
        sourceRow >= 0 && sourceRow < 5 &&
        sourceColumn >= 0 && sourceColumn < 5 &&
        values(sourceRow * 5 + sourceColumn)
      }

    var row = 0
    while row < 5 do
      var column = 0
      while column < 5 do
        assertEquals(
          output.data(row, column),
          reference(row, column),
          s"dilation diverged from reference at ($row, $column)"
        )
        column += 1
      row += 1

  /** Support.create canonically sorts offsets; sort taps identically so each
    * weight stays attached to its offset.
    */
  private def sparseKernel(taps: Vector[(Vector[Int], Double)]) =
    val sorted =
      taps.sortWith { (left, right) =>
        val comparison =
          left._1
            .zip(right._1)
            .map(Integer.compare(_, _))
            .find(_ != 0)
            .getOrElse(0)
        comparison < 0
      }
    val support =
      opsRight(Support.create[D2](sorted.map((offset, _) => Offset.unsafe[D2](offset))))
    opsRight(Kernel.sparse(support, sorted.map(_._2)))

  private def horizontalKernel =
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
    opsRight(Kernel.sparse(support, Vector(0.25, 0.5, 0.25)))

  private def doubleImage(values: Vector[Double], shape: Vector[Int]) =
    val frame = geometryRight(Frame.named[D2]("ops-law-filter"))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def maskImage(values: Vector[Boolean]) =
    val frame = geometryRight(Frame.named[D2]("ops-law-morphology"))
    val grid = geometryRight(Grid.in(frame)(Vector(5, 5), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(Sampled.mask(space, NDArray.fromSeq(Shape(5, 5), values)))

  private def opsRight[A](value: Either[OpError, A]): A =
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
