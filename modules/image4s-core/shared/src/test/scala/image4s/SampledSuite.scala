package image4s

import scala.compiletime.testing.typeCheckErrors

import munit.FunSuite
import ravel.AnyRank
import ravel.DType.given
import ravel.MutableNDArray
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Index

final class SampledSuite extends FunSuite:
  test("D2 scalar data preserves its statically known Ravel rank"):
    val frame = geometryRight(Frame.named[D2]("plane"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3), Affine.identity[D2]))
    val data = NDArray.fromSeq(Shape(2, 3), 0 until 6 map (_.toDouble))
    val sampled: ScalarImage[frame.type, D2, Rank[2]] =
      imageRight(Sampled.scalar(grid, NonSpatialAxes.empty, data))

    assertEquals(sampled.logicalShape, Vector(2, 3))
    assertEquals(sampled.data.shape.rank, 2)
    assert(sampled.data eq data)

  test("D3 time and channel axes remain explicit and non-spatial"):
    val frame = geometryRight(Frame.named[D3]("series"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3, 4), Affine.identity[D3]))
    val time = imageRight(Axis.create("time", 5, AxisKind.Time))
    val channel = imageRight(Axis.create("channel", 2, AxisKind.Channel))
    val axes = imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val data =
      NDArray.fromSeq(
        dynamicShape(2, 3, 4, 5, 2),
        0 until 240 map (_.toDouble)
      )
    val sampled: ScalarImage[frame.type, D3, AnyRank] =
      imageRight(Sampled.scalar(grid, axes, data))

    assertEquals(sampled.grid.spatialRank, 3)
    assertEquals(sampled.nonSpatialAxes.shape, Vector(5, 2))
    assertEquals(sampled.logicalShape, Vector(2, 3, 4, 5, 2))
    assertEquals(sampled.data.shape.rank, 5)

  test("sampled metadata stays on the sole sampled owner across views and copies"):
    val frame = geometryRight(Frame.named[D3]("metadata"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3])
      )
    val time = imageRight(Axis.create("time", 3, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val data = NDArray.zeros[Double](2, 2, 2, 3)
    val metadata = ImageMetadata.named("subject-01")
    val sampled =
      imageRight(Sampled.scalar(grid, axes, data, metadata))
    val selected = imageRight(sampled.selectTime(1))
    val copied = sampled.materializedCopy

    assertEquals(sampled.metadata, metadata)
    assertEquals(selected.metadata, metadata)
    assertEquals(copied.metadata, metadata)
    assertEquals(selected.data(0, 0, 0), sampled.data(0, 0, 0, 1))
    assert(sampled.withMetadata(metadata) eq sampled)
    assertEquals(
      sampled.withMetadata(ImageMetadata.named("renamed")).metadata.label,
      "renamed"
    )

  test("sampled equality follows geometry, metadata, shape, and values"):
    val frame = geometryRight(Frame.named[D2]("equality"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2), Affine.identity[D2])
      )
    val metadata = ImageMetadata.named("same")
    val first =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Seq(1, 2, 3, 4)),
          metadata
        )
      )
    val equalCopy = first.materializedCopy
    val differentValues =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Seq(1, 2, 3, 5)),
          metadata
        )
      )

    assertEquals(first, equalCopy)
    assertEquals(first.hashCode(), equalCopy.hashCode())
    assertNotEquals(first, differentValues)
    assertNotEquals(
      first,
      first.withMetadata(ImageMetadata.named("different"))
    )

  test("sample-space axis edits retain the exact grid and frame owners"):
    val frame = geometryRight(Frame.named[D3]("axis-edits"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3, 4), Affine.identity[D3])
      )
    val time = imageRight(Axis.create("time", 5, AxisKind.Time))
    val echo = imageRight(Axis.create("echo", 2, AxisKind.Echo))
    val channel =
      imageRight(Axis.create("channel", 7, AxisKind.Channel))
    val base = SampleSpace.create(grid, NonSpatialAxes.empty)
    val withTime = imageRight(base.appendNonSpatial(time))
    val withEcho = imageRight(withTime.appendNonSpatial(echo))
    val updated = imageRight(withEcho.updateNonSpatial(1, channel))
    val removed = imageRight(updated.removeNonSpatial(0))

    assert(withTime.grid eq grid)
    assert(withEcho.grid.frame eq frame)
    assertEquals(
      withEcho.nonSpatialAxes.values.map(_.kind),
      Vector(AxisKind.Time, AxisKind.Echo)
    )
    assertEquals(
      updated.nonSpatialAxes.values.map(_.kind),
      Vector(AxisKind.Time, AxisKind.Channel)
    )
    assertEquals(removed.nonSpatialAxes.shape, Vector(7))
    assert(removed.grid eq grid)
    assert(removed.spatialOnly.grid eq grid)
    assertEquals(removed.spatialOnly.nonSpatialAxes.size, 0)

  test("shape validation uses grid shape followed by non-spatial axes"):
    val frame = geometryRight(Frame.named[D2]("shape"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val time = imageRight(Axis.create("time", 3, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val wrong = NDArray.zeros[Double](2, 2)

    assertEquals(
      Sampled.scalar(grid, axes, wrong),
      Left(
        ImageError.SampledShapeMismatch(
          Vector(2, 2, 3),
          Vector(2, 2)
        )
      )
    )

  test("all convenience image names are exact aliases of Sampled"):
    val frame = geometryRight(Frame.named[D2]("aliases"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val data = NDArray.zeros[Double](2, 2)
    val sampled =
      imageRight(Sampled.scalar(grid, NonSpatialAxes.empty, data))
    val image: Image[frame.type, D2, Double, Scalar, Rank[2]] = sampled
    val series: ImageSeries[
      frame.type,
      D2,
      Double,
      Scalar,
      Rank[2]
    ] = sampled
    val scalar: ScalarImage[frame.type, D2, Rank[2]] = sampled

    assert(image eq sampled)
    assert(series eq sampled)
    assert(scalar eq sampled)
    assert(image.data eq data)

  test("existential SampleSpace refinement preserves live geometry owners"):
    val frame2 = geometryRight(Frame.named[D2]("refine-d2"))
    val grid2 =
      geometryRight(Grid.in(frame2)(Vector(2, 3), Affine.identity[D2]))
    val space2: SomeSampleSpace =
      SampleSpace.create(grid2, NonSpatialAxes.empty)
    val refined2 = imageRight(space2.requireD2)

    assert(refined2 eq space2)
    assert(refined2.grid eq grid2)
    assertEquals(
      space2.requireD3,
      Left(ImageError.SpatialDimensionMismatch(3, 2))
    )

    val frame3 = geometryRight(Frame.named[D3]("refine-d3"))
    val grid3 =
      geometryRight(
        Grid.in(frame3)(Vector(2, 3, 4), Affine.identity[D3])
      )
    val space3: SomeSampleSpace =
      SampleSpace.create(grid3, NonSpatialAxes.empty)
    val refined3 = imageRight(space3.requireD3)

    assert(refined3 eq space3)
    assert(refined3.grid eq grid3)
    assertEquals(
      space3.requireD2,
      Left(ImageError.SpatialDimensionMismatch(2, 3))
    )

  test("mutable Ravel input is copied at the explicit ownership boundary"):
    val frame = geometryRight(Frame.named[D2]("mutable-input"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val mutable = MutableNDArray.zeros[Double, Rank[2]](Shape(2, 2))
    mutable.update(0, 0, 7.0)
    val sampled =
      imageRight(
        Sampled.copyScalarFromMutable(
          grid,
          NonSpatialAxes.empty,
          mutable
        )
      )
    mutable.update(0, 0, 99.0)

    assertEquals(imageRight(sampled.valueAt(Vector(0, 0))), 7.0)

  test("non-spatial axes reject invalid names, extents, and duplicates"):
    assertEquals(
      Axis.create(" time", 3, AxisKind.Time),
      Left(ImageError.InvalidAxisName(" time"))
    )
    assertEquals(
      Axis.create("time", 0, AxisKind.Time),
      Left(ImageError.NonPositiveAxisExtent("time", 0))
    )
    val first = imageRight(Axis.create("time", 3, AxisKind.Time))
    val second = imageRight(Axis.create("time", 2, AxisKind.Time))
    assertEquals(
      NonSpatialAxes.from(Vector(first, second)),
      Left(ImageError.DuplicateAxisName("time"))
    )

  test("value lookup reports structured spatial and non-spatial errors"):
    val frame = geometryRight(Frame.named[D2]("lookup"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val data =
      NDArray.zeros[Double, Rank[3]](Shape(2, 2, 2))
    val sampled = imageRight(Sampled.scalar(grid, axes, data))

    assertEquals(
      sampled.valueAt(Vector(0), Vector(0)),
      Left(ImageError.SpatialIndexRankMismatch(2, 1))
    )
    assertEquals(
      sampled.valueAt(Vector(2, 0), Vector(0)),
      Left(ImageError.SpatialIndexOutOfBounds(0, 2, 2))
    )
    assertEquals(
      sampled.valueAt(Vector(0, 0), Vector.empty),
      Left(ImageError.NonSpatialIndexRankMismatch(1, 0))
    )
    assertEquals(
      sampled.valueAt(Vector(0, 0), Vector(2)),
      Left(
        ImageError.NonSpatialIndexOutOfBounds(
          imageRight(AxisName.parse("time")),
          2,
          2
        )
      )
    )

  test("ranked logical apply delegates directly to Ravel rank 2, 3, and 4"):
    val planeFrame = geometryRight(Frame.named[D2]("ranked-plane"))
    val planeGrid =
      geometryRight(
        Grid.in(planeFrame)(Vector(2, 3), Affine.identity[D2])
      )
    val plane =
      imageRight(
        Sampled.scalar(
          planeGrid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3)((i, j) =>
            10.0 * i.toDouble + j.toDouble
          )
        )
      )
    assertEquals(plane(1, 2), 12.0)

    val frame = geometryRight(Frame.named[D3]("ranked-volume"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3, 4), Affine.identity[D3])
      )
    val volume =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3, 4)((i, j, k) =>
            100.0 * i.toDouble + 10.0 * j.toDouble + k.toDouble
          )
        )
      )
    assertEquals(volume(1, 2, 3), 123.0)

    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val series =
      imageRight(
        Sampled.scalar(
          grid,
          axes,
          NDArray.tabulate[Double](2, 3, 4, 2)((i, j, k, t) =>
            1000.0 * t.toDouble +
              100.0 * i.toDouble +
              10.0 * j.toDouble +
              k.toDouble
          )
        )
      )
    assertEquals(series(1, 2, 3, 1), 1123.0)

    val erased =
      imageRight(
        Sampled.scalar(
          grid,
          axes,
          NDArray.fromSeq(
            dynamicShape(2, 3, 4, 2),
            for
              i <- 0 until 2
              j <- 0 until 3
              k <- 0 until 4
              t <- 0 until 2
            yield
              100.0 * i.toDouble +
                10.0 * j.toDouble +
                k.toDouble +
                1000.0 * t.toDouble
          )
        )
      )
    val refined = imageRight(erased.requireDataRank[4])
    assertEquals(refined(1, 2, 3, 1), 1123.0)
    assertEquals(
      erased.requireDataRank[3],
      Left(ImageError.StorageRankMismatch(3, 4))
    )

  test("time, channel, and direction selection are zero-copy rank drops"):
    val frame = geometryRight(Frame.named[D3]("axis-views"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3, 4), Affine.identity[D3])
      )
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val timeAxes = imageRight(NonSpatialAxes.from(Vector(time)))
    val series =
      imageRight(
        Sampled.scalar(
          grid,
          timeAxes,
          NDArray.tabulate[Double](2, 3, 4, 2)((i, j, k, t) =>
            1000.0 * t.toDouble +
              100.0 * i.toDouble +
              10.0 * j.toDouble +
              k.toDouble
          )
        )
      )
    val volume: ScalarImage[frame.type, D3, Rank[3]] =
      imageRight(series.selectTime(1))

    assert(volume.grid eq grid)
    assertEquals(volume.nonSpatialAxes.size, 0)
    assert(!volume.data.isWholeBuffer)
    assertEquals(volume(1, 2, 3), series(1, 2, 3, 1))

    val direction =
      imageRight(Axis.create("direction", 3, AxisKind.Direction))
    val directionAxes =
      imageRight(NonSpatialAxes.from(Vector(direction)))
    val components =
      imageRight(
        Sampled.components(
          grid,
          directionAxes,
          NDArray.tabulate[Double](2, 3, 4, 3)((i, j, k, d) =>
            1000.0 * d.toDouble +
              100.0 * i.toDouble +
              10.0 * j.toDouble +
              k.toDouble
          )
        )
      )
    val component = imageRight(components.selectDirection(2))
    assertEquals(component.nonSpatialAxes.size, 0)
    assert(!component.data.isWholeBuffer)
    assertEquals(component(1, 2, 3), components(1, 2, 3, 2))

    val channel =
      imageRight(Axis.create("channel", 2, AxisKind.Channel))
    val mixedAxes =
      imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val mixedData =
      NDArray.fromSeq(
        dynamicShape(2, 3, 4, 2, 2),
        0 until 96 map (_.toDouble)
      )
    val mixed =
      imageRight(Sampled.scalar(grid, mixedAxes, mixedData))
    val atTime = imageRight(mixed.selectTime(1))
    val atTimeRanked = imageRight(atTime.requireDataRank[4])
    val rankedChannel =
      imageRight(atTimeRanked.selectChannel(1))

    assertEquals(atTime.nonSpatialAxes.values.map(_.kind), Vector(AxisKind.Channel))
    assertEquals(rankedChannel.nonSpatialAxes.size, 0)
    assert(!rankedChannel.data.isWholeBuffer)
    assertEquals(rankedChannel(1, 2, 3), 95.0)

  test("non-spatial selection reports axis, kind, and coordinate failures"):
    val frame = geometryRight(Frame.named[D3]("axis-errors"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3])
      )
    val volume =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](1, 1, 1)
        )
      )
    assertEquals(
      volume.selectTime(0),
      Left(ImageError.MissingNonSpatialAxisKind(AxisKind.Time))
    )
    assertEquals(
      volume.selectNonSpatial(0, 0),
      Left(ImageError.NonSpatialAxisOutOfBounds(0, 0))
    )

    val first = imageRight(Axis.create("time-a", 2, AxisKind.Time))
    val second = imageRight(Axis.create("time-b", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(first, second)))
    val ambiguous =
      imageRight(
        Sampled.scalar(
          grid,
          axes,
          NDArray.fromSeq(
            dynamicShape(1, 1, 1, 2, 2),
            0 until 4 map (_.toDouble)
          )
        )
      )
    assertEquals(
      ambiguous.selectTime(0),
      Left(ImageError.AmbiguousNonSpatialAxisKind(AxisKind.Time, 2))
    )
    assertEquals(
      ambiguous.selectNonSpatial(0, 2),
      Left(
        ImageError.NonSpatialIndexOutOfBounds(
          imageRight(AxisName.parse("time-a")),
          2,
          2
        )
      )
    )

  test("spatial views preserve values and shift the complete grid affine"):
    val frame = geometryRight(Frame.named[D3]("spatial-view"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D3](
          origin = Vector(10.0, 20.0, 30.0),
          spacing = Vector(2.0, 3.0, 4.0),
          directionRowMajor = Vector(
            0.0,
            0.0,
            1.0,
            0.0,
            1.0,
            0.0,
            1.0,
            0.0,
            0.0
          )
        )
      )
    val grid =
      geometryRight(Grid.in(frame)(Vector(5, 6, 7), affine))
    val source =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](5, 6, 7)((i, j, k) =>
            100.0 * i.toDouble + 10.0 * j.toDouble + k.toDouble
          )
        )
      )
    val view =
      imageRight(
        source.spatialView(
          origin = Vector(1, 2, 3),
          shape = Vector(3, 3, 2)
        )
      )
    val sourceOrigin =
      geometryRight(grid.pointAt(geometryRight(Index.of[D3](1, 2, 3))))
    val viewOrigin =
      geometryRight(view.grid.pointAt(geometryRight(Index.of[D3](0, 0, 0))))

    assertEquals(view.grid.shape, Vector(3, 3, 2))
    assertEquals(viewOrigin.coordinates, sourceOrigin.coordinates)
    assertEquals(view(0, 0, 0), source(1, 2, 3))
    assertEquals(view(2, 2, 1), source(3, 4, 4))
    assert(!view.data.isWholeBuffer)
    assertEquals(view.data.size, 18)

    assertEquals(
      source.spatialView(Vector(0, 0), Vector(1, 1)),
      Left(ImageError.SpatialViewRankMismatch(3, 2, 2))
    )
    assertEquals(
      source.spatialView(Vector(0, 0, 0), Vector(1, 0, 1)),
      Left(ImageError.NonPositiveSpatialViewExtent(1, 0))
    )
    assertEquals(
      source.spatialView(Vector(4, 0, 0), Vector(2, 1, 1)),
      Left(ImageError.SpatialViewOutOfBounds(0, 4, 2, 5))
    )

  test("canonical layout and materialized copy make copy behavior explicit"):
    val frame = geometryRight(Frame.named[D3]("materialization"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(3, 2, 2), Affine.identity[D3])
      )
    val base =
      NDArray.tabulate[Double](3, 2, 2)((i, j, k) =>
        100.0 * i.toDouble + 10.0 * j.toDouble + k.toDouble
      )
    val canonical =
      imageRight(Sampled.scalar(grid, NonSpatialAxes.empty, base))
    val reversed =
      imageRight(
        Sampled.scalar(grid, NonSpatialAxes.empty, base.reverse(0))
      )
    val normalized = reversed.canonicalLayout
    val copied = canonical.materializedCopy

    assert(canonical.canonicalLayout eq canonical)
    assert(normalized.data.isCanonicalLayout)
    assert(normalized.data.isWholeBuffer)
    assert(normalized.data ne reversed.data)
    assertEquals(normalized(0, 1, 1), reversed(0, 1, 1))
    assert(copied.data ne canonical.data)
    assert(copied.data.isCanonicalLayout)
    assert(copied.data.sameElements(canonical.data))

  test("partial validity weights admit only the open finite unit interval"):
    assert(PartialWeight.from(Double.NaN).isLeft)
    assert(PartialWeight.from(Double.PositiveInfinity).isLeft)
    assert(PartialWeight.from(-0.1).isLeft)
    assert(PartialWeight.from(0.0).isLeft)
    assert(PartialWeight.from(1.0).isLeft)
    assert(PartialWeight.from(1.1).isLeft)
    assertEquals(imageRight(PartialWeight.from(0.4)).value, 0.4)

  test("SomeSampled folds a D2 value without copying or erasing its owner"):
    val frame = geometryRight(Frame.named[D2]("dynamic-plane"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](2, 3)
        )
      )
    val discovered = SomeSampled.d2(sampled)

    val shape =
      discovered.fold(
        d2 =>
          assert(d2.value eq sampled)
          assertEquals(d2.dimension.rank, 2)
          assertEquals(d2.value.frame.id, frame.id)
          d2.value.logicalShape,
        _ => fail("expected D2")
      )

    assertEquals(shape, Vector(2, 3))
    assertEquals(discovered.spatialRank, 2)
    assertEquals(discovered.storageRank, 2)

  test("SomeSampled folds a D3 value while retaining its hidden rank"):
    val frame = geometryRight(Frame.named[D3]("dynamic-volume"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3]))
    val time = imageRight(Axis.create("time", 3, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val sampled =
      imageRight(
        Sampled.scalar(
          grid,
          axes,
          NDArray.zeros[Double, Rank[4]](Shape(2, 2, 2, 3))
        )
      )
    val discovered: SomeSampled[Double, Scalar] =
      SomeSampled.d3(sampled)

    val ranks =
      discovered.fold(
        _ => fail("expected D3"),
        d3 =>
          assert(d3.value eq sampled)
          d3.dimension.rank -> d3.value.data.shape.rank
      )

    assertEquals(ranks, 3 -> 4)

  test("SomeSampled constructors reject the wrong spatial dimension"):
    val errors = typeCheckErrors(
      """
import image4s.*
import ravel.NDArray
import image4s.geometry.*
val frame = Frame.named[D3]("volume").toOption.get
val grid = Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]).toOption.get
val sampled = Sampled
  .scalar(grid, NonSpatialAxes.empty, NDArray.zeros[Double](1, 1, 1))
  .toOption
  .get
SomeSampled.d2(sampled)
"""
    )

    assert(errors.nonEmpty)

    val extensionErrors = typeCheckErrors(
      """
import image4s.*
trait UnsupportedPackage extends SomeSampled[Double, Scalar]
"""
    )

    assert(
      extensionErrors.exists(_.message.toLowerCase.contains("sealed"))
    )

  test("fast apply requires a statically refined total rank"):
    val erasedErrors = typeCheckErrors(
      """
import image4s.*
import ravel.{AnyRank, NDArray}
import image4s.geometry.*
val frame = Frame.named[D3]("erased").toOption.get
val grid = Grid
  .in(frame)(Vector(1, 1, 1), Affine.identity[D3])
  .toOption
  .get
val dynamic = Shape.from(Vector(1, 1, 1)).toOption.get
val erased = Sampled
  .scalar(grid, NonSpatialAxes.empty, NDArray.zeros[Double, AnyRank](dynamic))
  .toOption
  .get
erased(0, 0, 0)
"""
    )
    val wrongArityErrors = typeCheckErrors(
      """
import image4s.*
import ravel.NDArray
import image4s.geometry.*
val frame = Frame.named[D3]("rank-four").toOption.get
val grid = Grid
  .in(frame)(Vector(1, 1, 1), Affine.identity[D3])
  .toOption
  .get
val time = Axis.create("time", 1, AxisKind.Time).toOption.get
val axes = NonSpatialAxes.from(Vector(time)).toOption.get
val series = Sampled
  .scalar(grid, axes, NDArray.zeros[Double](1, 1, 1, 1))
  .toOption
  .get
series(0, 0, 0)
"""
    )

    assert(erasedErrors.nonEmpty)
    assert(wrongArityErrors.nonEmpty)

  private def dynamicShape(dimensions: Int*): Shape[AnyRank] =
    Shape.from(dimensions) match
      case Right(shape) => shape
      case Left(error)  => fail(error.toString)

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
