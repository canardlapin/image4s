package image4s

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

import scala.compiletime.testing.typeCheckErrors

final class SamplingAlignmentSuite extends FunSuite:
  test("same live owner operations are total and ephemeral identity is explicit"):
    val frame = geometryRight(Frame.named[D2]("same-owner"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(Shape(2, 2), Vector(1.0, 2.0, 3.0, 4.0))
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(Shape(2, 2), Vector(5.0, 6.0, 7.0, 8.0))
        )
      )
    val combined =
      left.zipWithAs[Continuous, Double, Continuous](right)(_ + _)

    assert(left.sameRuntimeSpaceAs(right))
    assertEquals(
      left.samePersistentSpaceAs(right),
      Left(ImageError.PersistentSampleSpaceUnavailable(true, true))
    )
    assert(combined.sampleSpace eq space)
    assertEquals(combined(1, 1), 12.0)

  test("different static owners cannot use total zipWith"):
    val errors = typeCheckErrors(
      """
import image4s.*
import ravel.{Rank, DType}
import ravel.DType.given
def invalid[
  L <: SampleSpace[?, ?],
  R <: SampleSpace[?, ?]
](
  left: Sampled[L, Double, Continuous, Rank[2]],
  right: Sampled[R, Double, Continuous, Rank[2]]
) =
  left.zipWithAs[Continuous, Double, Continuous](right)(_ + _)
"""
    )

    assert(errors.nonEmpty)

  test("restored persistent owners align, compose, and rebind zero-copy"):
    val originalFrame =
      geometryRight(
        Frame.persistentNamed[D2](
          frameId("alignment-frame"),
          "original",
          convention = CoordinateConvention.RAS
        )
      )
    val originalGrid =
      geometryRight(
        Grid.createPersistent(gridId("alignment-grid"), originalFrame)(
          Vector(2, 2),
          Affine.identity[D2]
        )
      )
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val originalSpace = SampleSpace.create(originalGrid, axes)
    val frameRecord = geometryRight(originalFrame.record)
    val gridRecord = geometryRight(originalGrid.record)
    val spaceRecord = imageRight(originalSpace.record)

    val frameA =
      geometryRight(Frame.restore[D2](frameRecord, Frame.Registry.empty)).frame
    val frameB =
      geometryRight(Frame.restore[D2](frameRecord, Frame.Registry.empty)).frame
    val frameC =
      geometryRight(Frame.restore[D2](frameRecord, Frame.Registry.empty)).frame
    val gridA =
      geometryRight(
        Grid.restore(gridRecord, frameA, Grid.Registry.empty)
      ).grid
    val gridB =
      geometryRight(
        Grid.restore(gridRecord, frameB, Grid.Registry.empty)
      ).grid
    val gridC =
      geometryRight(
        Grid.restore(gridRecord, frameC, Grid.Registry.empty)
      ).grid
    val spaceA = imageRight(SampleSpace.restore(spaceRecord, gridA))
    val spaceB = imageRight(SampleSpace.restore(spaceRecord, gridB))
    val spaceC = imageRight(SampleSpace.restore(spaceRecord, gridC))
    val alignmentAB = imageRight(spaceA.alignExact(spaceB))
    val alignmentBC = imageRight(spaceB.alignExact(spaceC))
    val alignmentAC = alignmentAB.andThen(alignmentBC)
    val data =
      NDArray.fromSeq(
        Shape(2, 2, 2),
        0 until 8 map (_.toDouble)
      )
    val left =
      imageRight(
        Sampled.continuous(
          spaceA,
          data,
          ImageMetadata.named("subject-a")
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          spaceB,
          NDArray.fromSeq(
            Shape(2, 2, 2),
            10 until 18 map (_.toDouble)
          )
        )
      )
    val rebound = left.rebind(alignmentAB)
    val roundTrip = rebound.rebind(alignmentAB.reverse)
    val selected = imageRight(left.selectTime(0))
    val combined =
      left.zipWithAlignedAs[
        spaceB.type,
        Continuous,
        Double,
        Continuous
      ](right, alignmentAB)(_ + _)
    val difference =
      left.zipWithAlignedAs[
        spaceB.type,
        Continuous,
        Double,
        Continuous
      ](right, alignmentAB)(_ - _)

    assert(spaceA ne spaceB)
    assertEquals(spaceA.persistentRelationTo(spaceB), PersistentSpaceComparison.Same)
    assertEquals(spaceA.samePersistentSpaceAs(spaceB), Right(true))
    assert(!spaceA.sameRuntimeSpaceAs(spaceB))
    assert(alignmentAC.left eq spaceA)
    assert(alignmentAC.right eq spaceC)
    assert(rebound.sampleSpace eq spaceB)
    assert(rebound.data eq left.data)
    assertEquals(rebound.metadata, left.metadata)
    assert(roundTrip.sampleSpace eq spaceA)
    assert(roundTrip.data eq left.data)
    assertEquals(
      rebound.sharesStorageWith(left),
      StorageSharing.SameArrayObject
    )
    assertEquals(
      selected.sharesStorageWith(left),
      StorageSharing.Unknown
    )
    assertEquals(
      combined.data.elementsIterator.toVector,
      Vector.tabulate(8)(index => 10.0 + 2.0 * index.toDouble)
    )
    assertEquals(
      difference.data.elementsIterator.toVector,
      Vector.fill(8)(-10.0)
    )

  test("persistent identity and exact congruence are distinct relations"):
    val frame =
      geometryRight(
        Frame.persistentNamed[D2](
          frameId("congruent-frame"),
          "congruent"
        )
      )
    val leftGrid =
      geometryRight(
        Grid.createPersistent(gridId("congruent-grid-left"), frame)(
          Vector(2, 2),
          Affine.identity[D2]
        )
      )
    val rightGrid =
      geometryRight(
        Grid.createPersistent(gridId("congruent-grid-right"), frame)(
          Vector(2, 2),
          Affine.identity[D2]
        )
      )
    val left = SampleSpace.create(leftGrid, NonSpatialAxes.empty)
    val right = SampleSpace.create(rightGrid, NonSpatialAxes.empty)

    assertEquals(
      left.persistentRelationTo(right),
      PersistentSpaceComparison.Different
    )
    assertEquals(left.samePersistentSpaceAs(right), Right(false))
    assert(imageRight(left.alignExact(right)).left eq left)

  test("approximate grid congruence always requires an explicit tolerance"):
    val frame = geometryRight(Frame.named[D2]("approximate"))
    val leftGrid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val shifted =
      geometryRight(
        Affine.fromRowMajor[D2](
          Vector(
            1.0, 0.0, 1e-6, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0
          )
        )
      )
    val rightGrid =
      geometryRight(Grid.in(frame)(Vector(2, 2), shifted))
    val left = SampleSpace.create(leftGrid, NonSpatialAxes.empty)
    val right = SampleSpace.create(rightGrid, NonSpatialAxes.empty)

    assert(left.alignExact(right).isLeft)
    assert(left.approximatelyCongruentTo(right, 1e-7).isLeft)
    assertEquals(
      imageRight(left.approximatelyCongruentTo(right, 1e-5)).tolerance,
      1e-5
    )

  test("unit, coordinate, and frame-convention mismatches remain explicit"):
    val ras =
      geometryRight(
        Frame.named[D2](
          "ras",
          convention = CoordinateConvention.RAS
        )
      )
    val lps =
      geometryRight(
        Frame.named[D2](
          "lps",
          convention = CoordinateConvention.LPS
        )
      )
    val rasGrid =
      geometryRight(Grid.in(ras)(Vector(1, 1), Affine.identity[D2]))
    val lpsGrid =
      geometryRight(Grid.in(lps)(Vector(1, 1), Affine.identity[D2]))
    val seconds =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.0,
          AxisUnit.Seconds
        )
      )
    val milliseconds =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1000.0,
          AxisUnit.Milliseconds
        )
      )
    val differentStep =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          2.0,
          AxisUnit.Seconds
        )
      )
    val secondsAxes = imageRight(NonSpatialAxes.from(Vector(seconds)))
    val millisAxes = imageRight(NonSpatialAxes.from(Vector(milliseconds)))
    val stepAxes = imageRight(NonSpatialAxes.from(Vector(differentStep)))
    val baseline = SampleSpace.create(rasGrid, secondsAxes)
    val unitMismatch = SampleSpace.create(rasGrid, millisAxes)
    val coordinateMismatch = SampleSpace.create(rasGrid, stepAxes)
    val conventionMismatch = SampleSpace.create(lpsGrid, secondsAxes)

    assertEquals(
      baseline.alignExact(unitMismatch),
      Left(
        ImageError.NonSpatialSamplingMismatch(
          secondsAxes.records,
          millisAxes.records
        )
      )
    )
    assertEquals(
      baseline.alignExact(coordinateMismatch),
      Left(
        ImageError.NonSpatialSamplingMismatch(
          secondsAxes.records,
          stepAxes.records
        )
      )
    )
    assert(baseline.alignExact(conventionMismatch).isLeft)

  test("descriptive metadata never controls sampling alignment"):
    val frame = geometryRight(Frame.named[D2]("metadata"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val original =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.zeros[Double](1, 1),
          ImageMetadata.named("original")
        )
      )
    val renamed = original.withMetadata(ImageMetadata.named("renamed"))

    assert(original ne renamed)
    assert(original.sameRuntimeSpaceAs(renamed))
    val identity = SamplingAlignment.identity(space)
    assert(identity.left eq space)
    assert(identity.right eq space)

  test("NaN and signed-zero behavior exists only in named value relations"):
    val frame = geometryRight(Frame.named[D2]("floating-values"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 2), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(Shape(1, 2), Vector(Double.NaN, -0.0))
        )
      )
    val right =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.fromSeq(Shape(1, 2), Vector(Double.NaN, 0.0))
        )
      )

    assertNotEquals(left, right)
    assert(!left.sameValuesAs(right)(_ == _))
    assert(
      left.sameValuesAs(right)((a, b) => (a.isNaN && b.isNaN) || a == b)
    )
    assert(
      !left.sameValuesAs(right)((a, b) =>
        java.lang.Double.doubleToRawLongBits(a) ==
          java.lang.Double.doubleToRawLongBits(b)
      )
    )

  test("hashing a representative 4D image is identity-based"):
    val frame = geometryRight(Frame.named[D3]("hash-map"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(24, 24, 12), Affine.identity[D3])
      )
    val time = imageRight(Axis.create("time", 20, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val space = SampleSpace.create(grid, axes)
    val image =
      imageRight(
        Sampled.continuous(
          space,
          NDArray.zeros[Double](24, 24, 12, 20)
        )
      )
    val copy = image.materializedCopy
    val indexed: Map[AnyRef, String] =
      Map(image -> "image", copy -> "copy")

    assertEquals(image.hashCode(), System.identityHashCode(image))
    assertEquals(copy.hashCode(), System.identityHashCode(copy))
    assertEquals(indexed.size, 2)

  private def frameId(value: String): FrameId =
    geometryRight(FrameId.parse(value))

  private def gridId(value: String): GridId =
    geometryRight(GridId.parse(value))

  private def geometryRight[A](
      value: Either[GeometryError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
