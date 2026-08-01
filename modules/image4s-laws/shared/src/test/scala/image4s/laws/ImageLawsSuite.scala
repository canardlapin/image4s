package image4s.laws

import image4s.Axis
import image4s.AxisKind
import image4s.AxisUnit
import image4s.ImageError
import image4s.ImageMetadata
import image4s.LatticeMap
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.SampleSpace
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.ContinuousIndex
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LatticeIndex
import image4s.geometry.Point
import image4s.geometry.Vec

final class ImageLawsSuite extends ScalaCheckSuite:
  private val frame =
    geometryRight(Frame.named[D2]("image-law-frame"))

  property("every constructed Sampled value obeys its shape law"):
    forAll(
      Gen.choose(1, 8),
      Gen.choose(1, 8)
    ): (firstExtent, secondExtent) =>
      val grid =
        geometryRight(
          Grid.in(frame)(
            Vector(firstExtent, secondExtent),
            Affine.identity[D2]
          )
        )
      val data =
        NDArray.zeros[Double, Rank[2]](
          Shape(firstExtent, secondExtent)
        )
      val sampled =
        imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, data))

      assert(ImageLaws.shapeAgrees(sampled))
      assert(sampled.data eq data)

  property("affine, point/vector, grid, and owner transports obey geometry laws"):
    forAll(
      Gen.choose(-20.0, 20.0),
      Gen.choose(-20.0, 20.0),
      Gen.choose(0.25, 5.0),
      Gen.choose(0.25, 5.0),
      Gen.choose(-5.0, 5.0),
      Gen.choose(-5.0, 5.0)
    ): (tx, ty, sx, sy, x, y) =>
      val affine =
        geometryRight(
          Affine.fromOriginSpacingDirection[D2](
            Vector(tx, ty),
            Vector(sx, sy),
            Vector(1.0, 0.0, 0.0, 1.0)
          )
        )
      val second =
        geometryRight(
          Affine.fromOriginSpacingDirection[D2](
            Vector(-ty, tx),
            Vector(1.5, 0.75),
            Vector(0.0, -1.0, 1.0, 0.0)
          )
        )
      val third =
        geometryRight(
          Affine.fromOriginSpacingDirection[D2](
            Vector(1.0, -2.0),
            Vector(0.5, 2.0),
            Vector(1.0, 0.0, 0.0, 1.0)
          )
        )
      val coordinates = Vector(x, y)
      assert(geometryRight(GeometryLaws.affineIdentity[D2](coordinates, 1e-12)))
      assert(
        geometryRight(
          GeometryLaws.affineInverseRoundTrip(
            affine,
            coordinates,
            1e-9
          )
        )
      )
      assert(
        geometryRight(
          GeometryLaws.affineComposition(
            affine,
            second,
            coordinates,
            1e-9
          )
        )
      )
      assert(
        geometryRight(
          GeometryLaws.affineCompositionAssociative(
            affine,
            second,
            third,
            coordinates,
            1e-8
          )
        )
      )

      val point = geometryRight(Point.in(frame)(x, y))
      val vector = geometryRight(Vec.in(frame)(tx, ty))
      val secondVector = geometryRight(Vec.in(frame)(sx, sy))
      assert(
        geometryRight(
          GeometryLaws.pointTranslationRoundTrip(
            point,
            vector,
            1e-12
          )
        )
      )
      assert(
        GeometryLaws.pointTranslationComposition(
          point,
          vector,
          secondVector,
          1e-12
        )
      )
      val grid =
        geometryRight(Grid.in(frame)(Vector(5, 7), affine))
      val continuous =
        geometryRight(ContinuousIndex.of[D2](x, y))
      assert(
        geometryRight(
          GeometryLaws.gridContinuousRoundTrip(
            grid,
            continuous,
            1e-9
          )
        )
      )
      val frameAlignment = geometryRight(Frame.align(frame, frame))
      val gridAlignment = geometryRight(Grid.align(grid, grid))
      assert(
        geometryRight(
          GeometryLaws.frameAlignmentRoundTrip(
            frameAlignment,
            point,
            1e-12
          )
        )
      )
      assert(
        geometryRight(
          GeometryLaws.gridAlignmentRoundTrip(
            gridAlignment,
            point,
            1e-12
          )
        )
      )

  property("axis records, coordinates, and permutations obey their laws"):
    forAll(
      Gen.choose(1, 12),
      Gen.choose(-20.0, 20.0),
      Gen.oneOf(
        Gen.choose(-5.0, -0.1),
        Gen.choose(0.1, 5.0)
      )
    ): (extent, origin, step) =>
      val axes =
        Vector(
          imageRight(
            Axis.ordinal("trial", AxisKind.Batch, extent)
          ),
          imageRight(
            Axis.regular(
              "time",
              AxisKind.Time,
              extent,
              origin,
              step,
              AxisUnit.Seconds
            )
          ),
          imageRight(
            Axis.explicit(
              "frequency",
              AxisKind.custom("frequency").toOption.get,
              Vector.tabulate(extent)(index =>
                origin + index.toDouble * step * step
              ),
              AxisUnit.Hertz
            )
          ),
          imageRight(
            Axis.categorical(
              "channel",
              AxisKind.Channel,
              Vector.tabulate(extent)(index => s"channel-$index")
            )
          )
        )
      axes.foreach { axis =>
        assert(AxisLaws.coordinateCountMatchesExtent(axis))
        assert(AxisLaws.coordinateLookupMatchesRecord(axis))
        assert(AxisLaws.recordRoundTrip(axis))
      }
      val ordered = imageRight(NonSpatialAxes.from(axes))
      assert(
        AxisLaws.permutationRoundTrip(
          ordered,
          Vector(2, 0, 3, 1)
        )
      )

  property("field maps and exact pullbacks obey reusable algebraic laws"):
    forAll(
      Gen.choose(2, 8),
      Gen.choose(2, 8),
      Gen.choose(-10.0, 10.0),
      Gen.choose(-3.0, 3.0)
    ): (firstExtent, secondExtent, shift, scale) =>
      val grid =
        geometryRight(
          Grid.in(frame)(
            Vector(firstExtent, secondExtent),
            Affine.identity[D2]
          )
        )
      val sampled =
        imageRight(
          Sampled.continuous(
            grid,
            NonSpatialAxes.empty,
            NDArray.tabulate[Double](firstExtent, secondExtent) {
              (i, j) => 17.0 * i.toDouble + j.toDouble
            }
          )
        )
      val firstMap =
        imageRight(
          LatticeMap.crop[D2](
            grid.shape,
            Vector(0, 1),
            Vector(firstExtent, secondExtent - 1)
          )
        )
      val secondMap =
        imageRight(
          LatticeMap.flip[D2](firstMap.targetShape, axis = 0)
        )

      assert(ImageLaws.mapIdentity(sampled)(_ == _))
      assert(
        ImageLaws.mapComposition(
          sampled,
          _ + shift,
          _ * scale
        )(_ == _)
      )
      assert(ImageLaws.exactViewIdentity(sampled))
      assert(
        ImageLaws.exactViewComposition(sampled)(
          firstMap,
          secondMap
        )(_ == _)
      )
      val strided =
        imageRight(sampled.flipSpatial(axis = 0))
      assert(ImageLaws.canonicalLayoutIdempotent(strided))
      assert(
        ImageLaws.materializedCopyPreservesValues(strided)(_ == _)
      )

  test("sample-space alignments compose and aligned fields rebind zero-copy"):
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3), Affine.identity[D2])
      )
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val firstSpace = SampleSpace.create(grid, axes)
    val secondSpace = SampleSpace.create(grid, axes)
    val thirdSpace = SampleSpace.create(grid, axes)
    val firstToSecond =
      imageRight(firstSpace.alignExact(secondSpace))
    val secondToThird =
      imageRight(secondSpace.alignExact(thirdSpace))
    val left =
      imageRight(
        Sampled.continuous(
          firstSpace,
          NDArray.fromSeq(
            Shape(2, 3, 2),
            Vector.tabulate(12)(_.toDouble)
          ),
          ImageMetadata.named("left label")
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          secondSpace,
          NDArray.fromSeq(
            Shape(2, 3, 2),
            Vector.tabulate(12)(index => 100.0 + index)
          ),
          ImageMetadata.named("different descriptive label")
        )
      )

    assert(SampleSpaceLaws.logicalShapeAgrees(firstSpace))
    assert(SampleSpaceLaws.alignmentIdentity(firstSpace))
    assert(SampleSpaceLaws.alignmentReverse(firstToSecond))
    assert(
      SampleSpaceLaws.alignmentComposition(
        firstToSecond,
        secondToThird
      )
    )
    assert(ImageLaws.rebindSharesData(left, firstToSecond))
    val sameOwner = left.mapValues(_ + 100.0)
    assert(
      ImageLaws.zipPreservingAgreesPointwise(
        left,
        sameOwner,
        _ + _
      )(_ == _)
    )
    assert(
      ImageLaws.alignedZipPreservingAgreesPointwise(
        left,
        right,
        firstToSecond,
        _ + _
      )(_ == _)
    )
    assert(
      ImageLaws.alignedZipAgreesPointwise[
        firstSpace.type,
        secondSpace.type,
        Double,
        image4s.Continuous,
        image4s.Continuous,
        Double,
        image4s.Continuous,
        Rank[3]
      ](
        left,
        right,
        firstToSecond,
        _ + _
      )(_ == _)
    )

  property("reference sampling is exact on the lattice and affine in between"):
    forAll(
      Gen.choose(0, 4),
      Gen.choose(0, 4),
      Gen.choose(0.0, 3.999999),
      Gen.choose(0.0, 3.999999)
    ): (i, j, x, y) =>
      val grid =
        geometryRight(
          Grid.in(frame)(Vector(5, 5), Affine.identity[D2])
        )
      val sampled =
        imageRight(
          Sampled.continuous(
            grid,
            NonSpatialAxes.empty,
            NDArray.tabulate[Double](5, 5)((a, b) =>
              2.0 * a.toDouble - 3.0 * b.toDouble + 7.0
            )
          )
        )
      val lattice = geometryRight(LatticeIndex.of[D2](i, j))
      assert(
        imageRight(
          SamplingLaws.nearestAtIntegerIsExact(
            sampled,
            lattice
          )(_ == _)
        )
      )
      val point = geometryRight(Point.in(frame)(x, y))
      assert(
        imageRight(
          SamplingLaws.linearReproducesExpected(
            sampled,
            point,
            2.0 * x - 3.0 * y + 7.0,
            1e-10
          )
        )
      )

  test("allClose applies explicit absolute and relative tolerances"):
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(1, 3),
          Affine.identity[D2]
        )
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(
            Shape(1, 3),
            Vector(0.0, 1000.0, -1000.0)
          )
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(
            Shape(1, 3),
            Vector(0.05, 1000.5, -1000.5)
          )
        )
      )

    assert(
      !ImageLaws.allClose(
        left,
        right,
        tolerance(absolute = 0.01)
      )
    )
    assert(
      ImageLaws.allClose(
        left,
        right,
        tolerance(absolute = 0.1, relative = 0.001)
      )
    )

  test("allClose makes NaN, signed-zero, and infinity behavior explicit"):
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(1, 4),
          Affine.identity[D2]
        )
      )
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(
            Shape(1, 4),
            Vector(Double.NaN, -0.0, Double.PositiveInfinity, 1.0)
          )
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(
            Shape(1, 4),
            Vector(Double.NaN, 0.0, Double.PositiveInfinity, 1.0)
          )
        )
      )

    assert(
      !ImageLaws.allClose(
        left,
        right,
        tolerance(absolute = 0.0)
      )
    )
    assert(
      ImageLaws.allClose(
        left,
        right,
        tolerance(absolute = 0.0, equalNaN = true)
      )
    )

    val oppositeInfinity =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(
            Shape(1, 4),
            Vector(
              Double.NaN,
              0.0,
              Double.NegativeInfinity,
              1.0
            )
          )
        )
      )
    assert(
      !ImageLaws.allClose(
        left,
        oppositeInfinity,
        tolerance(absolute = 0.0, equalNaN = true)
      )
    )

  test("numeric tolerances reject negative and non-finite bounds"):
    assertEquals(
      NumericTolerance.create(-1.0),
      Left(ImageError.InvalidNumericTolerance(-1.0, 0.0))
    )
    NumericTolerance.create(Double.NaN) match
      case Left(ImageError.InvalidNumericTolerance(absolute, relative)) =>
        assert(absolute.isNaN)
        assertEquals(relative, 0.0)
      case other =>
        fail(s"expected invalid NaN tolerance, found $other")
    assertEquals(
      NumericTolerance.create(0.0, Double.PositiveInfinity),
      Left(
        ImageError.InvalidNumericTolerance(
          0.0,
          Double.PositiveInfinity
        )
      )
    )

  test("allCloseAligned consumes reusable exact sampling evidence"):
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(1, 2),
          Affine.identity[D2]
        )
      )
    val leftSpace = SampleSpace.create(grid, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Sampled.continuous(
          leftSpace,
          NDArray.fromSeq(Shape(1, 2), Vector(1.0, 2.0))
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          rightSpace,
          NDArray.fromSeq(Shape(1, 2), Vector(1.0, 2.0001))
        )
      )
    val alignment = imageRight(leftSpace.alignExact(rightSpace))
    val numericTolerance = tolerance(absolute = 0.001)

    assert(
      ImageLaws.allCloseAligned(
        left,
        right,
        alignment,
        numericTolerance
      )
    )
    assert(
      ImageLaws.allCloseAligned(
        left,
        right,
        alignment,
        numericTolerance
      )
    )

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def tolerance(
      absolute: Double,
      relative: Double = 0.0,
      equalNaN: Boolean = false
  ): NumericTolerance =
    imageRight(
      NumericTolerance.create(absolute, relative, equalNaN)
    )
