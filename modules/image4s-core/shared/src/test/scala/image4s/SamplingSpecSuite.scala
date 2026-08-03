package image4s

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.GeometryError
import image4s.geometry.LengthUnit
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

final class SamplingSpecSuite extends FunSuite:
  test("identity sampling binds spatial and regular-axis extents from one shape"):
    val specification =
      SamplingSpec[D3](
        frame = FrameSpec.named(
          "native",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.RAS
        ),
        grid = GridSpec.identity,
        axes = AxesSpec(
          AxisSpec.timeRegular(
            origin = 0.0,
            step = 0.8,
            unit = AxisUnit.Seconds
          )
        )
      )

    val space = imageRight(specification.buildFor(Shape(6, 7, 5, 4)))

    assertEquals(space.grid.shape, Vector(6, 7, 5))
    assertEquals(space.grid.indexToFrame.rowMajor, Affine.identity[D3].rowMajor)
    assertEquals(space.grid.frame.metadata.label, "native")
    assertEquals(space.grid.frame.unit, LengthUnit.Millimeter)
    assertEquals(space.grid.frame.convention, CoordinateConvention.RAS)
    assertEquals(space.nonSpatialAxes.shape, Vector(4))
    assertEquals(
      space.nonSpatialAxes.coordinateAt(0, 2),
      Right(AxisCoordinate.Numeric(1.6, AxisUnit.Seconds))
    )

  test("axis-aligned grid derives an affine without weakening geometry validation"):
    val specification =
      SamplingSpec[D2](
        frame = FrameSpec.named("plane", unit = LengthUnit.Micrometer),
        grid = GridSpec.axisAligned(
          origin = Vector(10.0, 20.0),
          spacing = Vector(2.0, 3.0)
        )
      )

    val space = imageRight(specification.buildFor(Vector(4, 5)))

    assertEquals(
      space.grid.indexToFrame.rowMajor,
      Vector(2.0, 0.0, 10.0, 0.0, 3.0, 20.0, 0.0, 0.0, 1.0)
    )

  test("caller-supplied affine is retained as the grid transform"):
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(1.0, 2.0),
          spacing = Vector(0.5, 0.75),
          directionRowMajor = Vector(0.0, -1.0, 1.0, 0.0)
        )
      )
    val specification =
      SamplingSpec[D2](
        FrameSpec.named("rotated"),
        GridSpec.affine(affine)
      )

    val space = imageRight(specification.buildFor(Shape(3, 4)))

    assert(space.grid.indexToFrame eq affine)

  test("explicit and categorical axes must agree with bound trailing extents"):
    val specification =
      SamplingSpec[D2](
        FrameSpec.named("axes"),
        GridSpec.identity,
        AxesSpec(
          AxisSpec.explicit(
            "echo",
            AxisKind.Echo,
            Vector(10.0, 20.0),
            AxisUnit.Milliseconds
          ),
          AxisSpec.categorical(
            "condition",
            AxisKind.Other,
            Vector("rest", "task", "control")
          )
        )
      )

    val space = imageRight(specification.buildFor(Shape(4, 5, 2, 3)))
    assertEquals(space.nonSpatialAxes.shape, Vector(2, 3))

    assertEquals(
      specification.buildFor(Shape(4, 5, 3, 3)),
      Left(
        ImageError.AxisSpecificationExtentMismatch(
          axis = 0,
          name = "echo",
          declaredExtent = 2,
          boundExtent = 3
        )
      )
    )

    assertEquals(
      specification.buildFor(Shape(4, 5, 2, 2)),
      Left(
        ImageError.AxisSpecificationExtentMismatch(
          axis = 1,
          name = "condition",
          declaredExtent = 3,
          boundExtent = 2
        )
      )
    )

  test("ordinal axes bind their extent and duplicate names remain invalid"):
    val ordinal =
      SamplingSpec[D2](
        FrameSpec.named("ordinal"),
        GridSpec.identity,
        AxesSpec.from(
          Vector(AxisSpec.ordinal("batch", AxisKind.Batch))
        )
      )
    val ordinalSpace = imageRight(ordinal.buildFor(Vector(2, 3, 4)))
    assertEquals(
      ordinalSpace.nonSpatialAxes.coordinateAt(0, 3),
      Right(AxisCoordinate.Ordinal(3))
    )

    val duplicate =
      SamplingSpec[D2](
        FrameSpec.named("duplicate"),
        GridSpec.identity,
        AxesSpec(
          AxisSpec.ordinal("run", AxisKind.Batch),
          AxisSpec.regular(
            "run",
            AxisKind.Time,
            origin = 0.0,
            step = 1.0,
            unit = AxisUnit.Seconds
          )
        )
      )
    assertEquals(
      duplicate.buildFor(Shape(2, 3, 4, 5)),
      Left(ImageError.DuplicateAxisName("run"))
    )

  test("storage rank must equal spatial rank plus declared axes"):
    val specification =
      SamplingSpec[D3](
        FrameSpec.named("ranked"),
        GridSpec.identity,
        AxesSpec(AxisSpec.ordinal("batch", AxisKind.Batch))
      )

    assertEquals(
      specification.buildFor(Shape(2, 3, 4)),
      Left(
        ImageError.SamplingSpecificationRankMismatch(
          spatialRank = 3,
          axisCount = 1,
          actualRank = 3
        )
      )
    )

  test("deferred frame and grid validation remains typed"):
    val invalidFrame =
      SamplingSpec[D2](FrameSpec.named(" bad "), GridSpec.identity)
    assertEquals(
      invalidFrame.buildFor(Shape(2, 2)),
      Left(ImageError.Geometry(GeometryError.InvalidFrameLabel(" bad ")))
    )

    val invalidGrid =
      SamplingSpec[D2](
        FrameSpec.named("bad-grid"),
        GridSpec.axisAligned(Vector(0.0), Vector(1.0, 1.0))
      )
    assertEquals(
      invalidGrid.buildFor(Shape(2, 2)),
      Left(ImageError.Geometry(GeometryError.DimensionMismatch(2, 1)))
    )

    val invalidSpacing =
      SamplingSpec[D2](
        FrameSpec.named("bad-spacing"),
        GridSpec.axisAligned(Vector(0.0, 0.0), Vector(1.0, 0.0))
      )
    assertEquals(
      invalidSpacing.buildFor(Shape(2, 2)),
      Left(ImageError.Geometry(GeometryError.InvalidSpacing(1, 0.0)))
    )

  test("persistent frame policy remains explicit"):
    val specification =
      SamplingSpec[D2](
        FrameSpec.persistent(
          id = "scanner-frame",
          label = "scanner",
          convention = CoordinateConvention.LPS
        ),
        GridSpec.identity
      )

    val space = imageRight(specification.buildFor(Shape(2, 3)))

    assertEquals(space.grid.frame.persistentId.map(_.value), Some("scanner-frame"))
    assertEquals(space.grid.frame.convention, CoordinateConvention.LPS)

    val invalid =
      SamplingSpec[D2](
        FrameSpec.persistent(" scanner-frame ", "scanner"),
        GridSpec.identity
      )
    assertEquals(
      invalid.buildFor(Shape(2, 3)),
      Left(
        ImageError.Geometry(
          GeometryError.InvalidFrameId(" scanner-frame ")
        )
      )
    )

  test("composite continuous construction is canonical and retains storage"):
    val specification =
      SamplingSpec[D3](
        FrameSpec.named(
          "composite",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.RAS
        ),
        GridSpec.identity,
        AxesSpec(
          AxisSpec.timeRegular(0.0, 2.0, AxisUnit.Seconds)
        )
      )
    val values =
      NDArray.tabulate[Double](2, 3, 4, 5)((i, j, k, t) => i + j + k + t)
    val metadata = ImageMetadata.named("synthetic")

    val direct =
      imageRight(Image.continuous(values, specification, metadata))
    val expandedSpace = imageRight(specification.buildFor(values.shape))
    val expanded =
      imageRight(Image.continuous(expandedSpace, values, metadata))

    assert(direct.data eq values)
    assert(expanded.data eq values)
    assertEquals(direct.logicalShape, expanded.logicalShape)
    assertEquals(
      direct.grid.indexToFrame.rowMajor,
      expanded.grid.indexToFrame.rowMajor
    )
    assertEquals(
      direct.nonSpatialAxes.records,
      expanded.nonSpatialAxes.records
    )
    assertEquals(direct.metadata, metadata)

  test("composite construction creates fresh owners while buildFor supports reuse"):
    val specification =
      SamplingSpec[D2](FrameSpec.named("owners"), GridSpec.identity)
    val leftValues = NDArray.tabulate[Double](2, 3)((i, j) => i + j)
    val rightValues = NDArray.tabulate[Double](2, 3)((i, j) => 10.0 + i + j)

    val capturedLeft = imageRight(Image.continuous(leftValues, specification))
    val capturedRight = imageRight(Image.continuous(rightValues, specification))
    assert(!(capturedLeft.sampleSpace eq capturedRight.sampleSpace))
    assertEquals(
      capturedLeft.sampleSpace.alignExact(capturedRight.sampleSpace),
      Left(ImageError.Geometry(GeometryError.EphemeralFrameMismatch))
    )

    val shared = imageRight(specification.buildFor(leftValues.shape))
    val left = imageRight(Image.continuous(shared, leftValues))
    val right = imageRight(Image.continuous(shared, rightValues))
    val combined = left.zipWith(right)(_ + _)

    assert(combined.sampleSpace eq shared)
    assertEquals(combined(1, 2), 16.0)

  test("composite construction preserves specification failures"):
    val values = NDArray.zeros[Double](2, 3)
    val invalid =
      SamplingSpec[D2](FrameSpec.named(" bad "), GridSpec.identity)

    assertEquals(
      Image.continuous(values, invalid),
      Left(ImageError.Geometry(GeometryError.InvalidFrameLabel(" bad ")))
    )

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
