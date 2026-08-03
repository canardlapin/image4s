package image4s

import scala.compiletime.testing.typeCheckErrors

import munit.FunSuite
import ravel.AnyRank
import ravel.ConversionError
import ravel.ConversionPolicy
import ravel.DType.given
import ravel.MutableNDArray
import ravel.NDArray
import ravel.Overflow
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.GridId
import image4s.geometry.LatticeIndex

final class SampledSuite extends FunSuite:
  test("D2 scalar data preserves its statically known Ravel rank"):
    val frame = geometryRight(Frame.named[D2]("plane"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3), Affine.identity[D2]))
    val data = NDArray.fromSeq(Shape(2, 3), 0 until 6 map (_.toDouble))
    val sampled =
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, data))

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
    val sampled =
      imageRight(Sampled.continuous(grid, axes, data))

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
      imageRight(Sampled.continuous(grid, axes, data, metadata))
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

  test("Sampled uses reference identity and named value comparison"):
    val frame = geometryRight(Frame.named[D2]("equality"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2), Affine.identity[D2])
      )
    val metadata = ImageMetadata.named("same")
    val first =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Seq(1.0, 2.0, 3.0, 4.0)),
          metadata
        )
      )
    val equalCopy = first.materializedCopy
    val differentValues =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Seq(1.0, 2.0, 3.0, 5.0)),
          metadata
        )
      )

    assert(first ne equalCopy)
    assertNotEquals(first, equalCopy)
    assert(
      first.sameValuesAs(equalCopy)(_ == _)
    )
    assert(
      !first.sameValuesAs(differentValues)(_ == _)
    )
    assertNotEquals(
      first,
      first.withMetadata(ImageMetadata.named("different"))
    )
    val indexed: Map[AnyRef, String] =
      Map(first -> "first", equalCopy -> "copy")
    assertEquals(indexed.size, 2)
    assertEquals(indexed(first), "first")

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

  test("sample-space structural comparison includes axis coordinates"):
    val frame = geometryRight(Frame.named[D3]("sampling-coordinates"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3])
      )
    val fast =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          240,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val slow =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          240,
          0.0,
          2.0,
          AxisUnit.Seconds
        )
      )
    val fastSpace =
      SampleSpace.create(
        grid,
        imageRight(NonSpatialAxes.from(Vector(fast)))
      )
    val slowSpace =
      SampleSpace.create(
        grid,
        imageRight(NonSpatialAxes.from(Vector(slow)))
      )

    assertNotEquals(fastSpace, slowSpace)

  test("SampleSpaceRecord separates persistent structure from live ownership"):
    val frame =
      geometryRight(
        Frame.persistentNamed[D2](
          geometryRight(FrameId.parse("sample-space-record-frame")),
          "sample-space-record"
        )
      )
    val grid =
      geometryRight(
        Grid.createPersistent(
          geometryRight(GridId.parse("sample-space-record-grid")),
          frame
        )(Vector(2, 2), Affine.identity[D2])
      )
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          1.5,
          AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val original = SampleSpace.create(grid, axes)
    val record = imageRight(original.record)
    val restored = imageRight(SampleSpace.restore(record, grid))

    assert(restored ne original)
    assert(restored.grid eq grid)
    assertEquals(restored.nonSpatialAxes.records, axes.records)
    assertEquals(imageRight(restored.record), record)

    val otherGrid =
      geometryRight(
        Grid.createPersistent(
          geometryRight(GridId.parse("sample-space-record-other-grid")),
          frame
        )(Vector(2, 2), Affine.identity[D2])
      )
    val otherRecord = geometryRight(otherGrid.record)
    assertEquals(
      SampleSpace.restore(record, otherGrid),
      Left(
        ImageError.SampleSpaceGridRecordMismatch(
          record.grid,
          otherRecord
        )
      )
    )

  test("shape validation uses grid shape followed by non-spatial axes"):
    val frame = geometryRight(Frame.named[D2]("shape"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val time = imageRight(Axis.create("time", 3, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val wrong = NDArray.zeros[Double](2, 2)

    assertEquals(
      Sampled.continuous(grid, axes, wrong),
      Left(
        ImageError.SampledShapeMismatch(
          Vector(2, 2, 3),
          Vector(2, 2)
        )
      )
    )

  test("generic continuous images remain the sole Sampled representation"):
    val frame = geometryRight(Frame.named[D2]("aliases"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val data = NDArray.zeros[Double](2, 2)
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val sampled =
      imageRight(Sampled.continuous(space, data))
    val image: Image[space.type, Double, Continuous, Rank[2]] = sampled
    val continuous: ContinuousImage[
      space.type,
      Double,
      Rank[2]
    ] = sampled

    assert(image eq sampled)
    assert(continuous eq sampled)
    assert(image.data eq data)

  test("unproven series and component aliases are absent"):
    val seriesErrors = typeCheckErrors(
      """
import image4s.*
type UncheckedSeries[F <: geometry.Frame[geometry.D2]] =
  ImageSeries[F, geometry.D2, Double, Continuous, ravel.AnyRank]
"""
    )
    val componentErrors = typeCheckErrors(
      """
import image4s.*
type UncheckedComponents[F <: geometry.Frame[geometry.D2]] =
  ComponentImage[F, geometry.D2, ravel.AnyRank]
"""
    )

    assert(seriesErrors.nonEmpty)
    assert(componentErrors.nonEmpty)

  test("MaskImage is a Boolean Mask alias distinct from Categorical labels"):
    val frame = geometryRight(Frame.named[D2]("mask-plane"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val data =
      NDArray.fromSeq(Shape(2, 2), Vector(true, false, true, false))
    val mask = imageRight(Sampled.mask(grid, NonSpatialAxes.empty, data))
    val asAlias: MaskImage[? <: SampleSpace[?, ?], Rank[2]] = mask
    val labels =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1L, 0L, 1L, 0L))
        )
      )

    assert(asAlias eq mask)
    assertEquals(mask(0, 0), true)
    assertEquals(mask(0, 1), false)
    assertEquals(labels(0, 0), 1L)
    val maskIsNotLabel =
      typeCheckErrors(
        """
import image4s.*
import ravel.Rank
def takeLabel[S <: SampleSpace[?, ?]](image: CategoricalImage[S, Boolean, Rank[2]]) = image
def check(mask: MaskImage[? <: SampleSpace[?, ?], Rank[2]]) = takeLabel(mask)
"""
      )
    assert(maskIsNotLabel.nonEmpty)

  test("mask factory rejects non-Boolean element types at compile time"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
val frame = Frame.named[D2]("plane").toOption.get
val grid = Grid.in(frame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val data = NDArray.fromSeq(Shape(2, 2), Vector(1, 0, 1, 0))
Sampled.mask(grid, NonSpatialAxes.empty, data)
"""
    )
    assert(errors.nonEmpty)

  test("Mask values do not receive LinearSampling evidence"):
    val errors = typeCheckErrors(
      """
import image4s.*
summon[LinearSampling[Boolean, Mask]]
"""
    )
    assert(errors.nonEmpty)

  test("DoubleContinuousImage and FloatContinuousImage are narrow Continuous aliases"):
    val frame = geometryRight(Frame.named[D2]("aliases"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val doubles =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](2, 2)
        )
      )
    val floats =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Float](2, 2)
        )
      )
    val asDouble: DoubleContinuousImage[? <: SampleSpace[?, ?], Rank[2]] =
      doubles
    val asFloat: FloatContinuousImage[? <: SampleSpace[?, ?], Rank[2]] =
      floats
    assert(asDouble eq doubles)
    assert(asFloat eq floats)

  test("semantic views cannot be constructed without checked axis evidence"):
    val timeSeriesErrors = typeCheckErrors(
      """
import image4s.*
import ravel.AnyRank
import image4s.geometry.*
def bypass[F <: Frame[D2], S <: SampleSpace[F, D2], A, Sem](
  image: Sampled[S, A, Sem, AnyRank],
  axis: Axis
) =
  new TimeSeriesView(image, 0, axis)
"""
    )
    val componentErrors = typeCheckErrors(
      """
import image4s.*
import ravel.AnyRank
import image4s.geometry.*
def bypass[F <: Frame[D2], S <: SampleSpace[F, D2], A, Sem](
  image: Sampled[S, A, Sem, AnyRank],
  axis: Axis
) =
  new ComponentAxisView(image, 0, axis)
"""
    )

    assert(timeSeriesErrors.nonEmpty)
    assert(componentErrors.nonEmpty)

  test("continuous values are generic while categorical integers stay concise"):
    val frame = geometryRight(Frame.named[D2]("value-semantics"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 2), Affine.identity[D2]))
    val continuous =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 2), Vector(1.5f, 2.5f))
        )
      )
    val categorical =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 2), Vector(3, 9))
        )
      )

    assertEquals(continuous(0, 1), 2.5f)
    assertEquals(categorical(0, 1), 9)

  test("categorical construction rejects non-integral element types"):
    val doubleErrors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
val frame = Frame.named[D2]("labels").toOption.get
val grid = Grid.in(frame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val data = NDArray.fromSeq(Shape(2, 2), Vector(1.0, 2.0, 3.0, 4.0))
Sampled.categorical(grid, NonSpatialAxes.empty, data)
"""
    )
    val booleanErrors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
val frame = Frame.named[D2]("labels").toOption.get
val grid = Grid.in(frame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val data = NDArray.fromSeq(Shape(2, 2), Vector(true, false, true, false))
Sampled.categorical(grid, NonSpatialAxes.empty, data)
"""
    )
    assert(doubleErrors.nonEmpty)
    assert(booleanErrors.nonEmpty)

  test("dtype forwards the Ravel element representation"):
    val frame = geometryRight(Frame.named[D2]("dtype"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val continuous =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Float](2, 2)
        )
      )
    val labels =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1, 2, 3, 4))
        )
      )
    val mask =
      imageRight(
        Sampled.mask(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(true, false, true, false))
        )
      )
    assertEquals(continuous.dtype.name, "Float")
    assertEquals(labels.dtype.name, "Int")
    assertEquals(mask.dtype.name, "Boolean")

  test("numeric conversion preserves image ownership and semantic role"):
    val frame = geometryRight(Frame.named[D2]("conversion"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val metadata = ImageMetadata.named("labels")
    val source =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1, 2, 3, 4)),
          metadata
        )
      )

    val converted = imageRight(source.convertTo[Byte]())

    assert(converted.sampleSpace eq source.sampleSpace)
    assert(converted.grid eq source.grid)
    assertEquals(converted.nonSpatialAxes, source.nonSpatialAxes)
    assertEquals(converted.metadata, source.metadata)
    assertEquals(converted.data.elementsIterator.toList, List[Byte](1, 2, 3, 4))

  test("numeric conversion exposes checked overflow and explicit clamp policy"):
    val frame = geometryRight(Frame.named[D2]("conversion-overflow"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 2), Affine.identity[D2]))
    val source =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(1, 2), Vector(127, 128))
        )
      )

    assertEquals(
      source.convertTo[Byte](),
      Left(
        ImageError.NumericConversion(
          ConversionError.OutOfRange(1, "Int", "Byte")
        )
      )
    )

    val clamped =
      imageRight(
        source.convertTo[Byte](ConversionPolicy(overflow = Overflow.Clamp))
      )
    assertEquals(
      clamped.data.elementsIterator.toList,
      List[Byte](127, Byte.MaxValue)
    )

  test("continuous construction requires a linear codomain"):
    val errors = typeCheckErrors(
      """
import image4s.*
import ravel.{NDArray, Shape}
import image4s.geometry.*
val frame = Frame.named[D2]("strings").toOption.get
val grid =
  Grid.in(frame)(Vector(1, 1), Affine.identity[D2]).toOption.get
Sampled.continuous(
  grid,
  NonSpatialAxes.empty,
  NDArray.fromSeq(Shape(1, 1), Vector("not-linear"))
)
"""
    )

    assert(errors.nonEmpty)

  test("generic construction requires explicit value-semantic evidence"):
    val errors = typeCheckErrors(
      """
import image4s.*
import ravel.{NDArray, Rank, Shape}
import image4s.geometry.*
sealed trait Unproven
val frame = Frame.named[D2]("unproven").toOption.get
val grid =
  Grid.in(frame)(Vector(1, 1), Affine.identity[D2]).toOption.get
Sampled.create[
  frame.type,
  D2,
  Double,
  Unproven,
  Rank[2]
](
  grid,
  NonSpatialAxes.empty,
  NDArray.fromSeq(Shape(1, 1), Vector(1.0))
)
"""
    )

    assert(errors.nonEmpty)

  test("downstream semantic tags opt in without extending a closed hierarchy"):
    sealed trait Probability
    given ValueSemantics[Double, Probability] with {}

    val frame = geometryRight(Frame.named[D2]("custom-semantics"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1), Affine.identity[D2]))
    val data = NDArray.fromSeq(Shape(1, 1), Vector(0.75))
    val sampled =
      imageRight(
        Sampled.create[
          frame.type,
          D2,
          Double,
          Probability,
          Rank[2]
        ](grid, NonSpatialAxes.empty, data)
      )

    assert(sampled.data eq data)
    assertEquals(sampled(0, 0), 0.75)

  test("existential SampleSpace inspection preserves live geometry owners"):
    val frame2 = geometryRight(Frame.named[D2]("refine-d2"))
    val grid2 =
      geometryRight(Grid.in(frame2)(Vector(2, 3), Affine.identity[D2]))
    val space2: SomeSampleSpace =
      SampleSpace.create(grid2, NonSpatialAxes.empty)
    assertEquals(space2.spatialRank, 2)
    assertEquals(space2.logicalShape, Vector(2, 3))
    assert(space2.typed.grid eq grid2)

    val frame3 = geometryRight(Frame.named[D3]("refine-d3"))
    val grid3 =
      geometryRight(
        Grid.in(frame3)(Vector(2, 3, 4), Affine.identity[D3])
      )
    val space3: SomeSampleSpace =
      SampleSpace.create(grid3, NonSpatialAxes.empty)
    assertEquals(space3.spatialRank, 3)
    assertEquals(space3.logicalShape, Vector(2, 3, 4))
    assert(space3.typed.grid eq grid3)

  test("mutable Ravel input is copied at the explicit ownership boundary"):
    val frame = geometryRight(Frame.named[D2]("mutable-input"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val mutable = MutableNDArray.zeros[Double, Rank[2]](Shape(2, 2))
    mutable.update(0, 0, 7.0)
    val sampled =
      imageRight(
        Sampled.copyContinuousFromMutable(
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
    val data =
      NDArray.zeros[Double, Rank[3]](Shape(2, 2, 2))
    val sampled = imageRight(Sampled.continuous(grid, axes, data))

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
        Sampled.continuous(
          planeGrid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3)((i, j) => 10.0 * i.toDouble + j.toDouble)
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
        Sampled.continuous(
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
        Sampled.continuous(
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
        Sampled.continuous(
          grid,
          axes,
          NDArray.fromSeq(
            dynamicShape(2, 3, 4, 2),
            for
              i <- 0 until 2
              j <- 0 until 3
              k <- 0 until 4
              t <- 0 until 2
            yield 100.0 * i.toDouble +
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
    val timeAxes = imageRight(NonSpatialAxes.from(Vector(time)))
    val series =
      imageRight(
        Sampled.continuous(
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
    val volume = imageRight(series.selectTime(1))

    assertEquals(
      time.coordinateAt(1),
      Right(AxisCoordinate.Numeric(0.8, AxisUnit.Seconds))
    )
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
        Sampled.continuous(
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
      imageRight(
        Axis.categorical(
          "channel",
          AxisKind.Channel,
          Vector("magnitude", "phase")
        )
      )
    val mixedAxes =
      imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val mixedData =
      NDArray.fromSeq(
        dynamicShape(2, 3, 4, 2, 2),
        0 until 96 map (_.toDouble)
      )
    val mixed =
      imageRight(Sampled.continuous(grid, mixedAxes, mixedData))
    val atTime = imageRight(mixed.selectTime(1))
    val selectedCoordinate =
      imageRight(mixed.nonSpatialAxes.coordinateAt(0, 1))
    val locatedAtTime =
      imageRight(mixed.selectNonSpatial(0, 1))
    val atTimeRanked = imageRight(atTime.requireDataRank[4])
    val rankedChannel =
      imageRight(atTimeRanked.selectChannel(1))

    assertEquals(
      selectedCoordinate,
      AxisCoordinate.Numeric(0.8, AxisUnit.Seconds)
    )
    assertEquals(
      locatedAtTime.nonSpatialAxes.records,
      Vector(channel.record)
    )
    assert(locatedAtTime.grid eq grid)
    assert(!locatedAtTime.data.isWholeBuffer)
    assertEquals(
      atTime.nonSpatialAxes.values.map(_.kind),
      Vector(AxisKind.Channel)
    )
    assertEquals(atTime.nonSpatialAxes.records, Vector(channel.record))
    assertEquals(rankedChannel.nonSpatialAxes.size, 0)
    assert(!rankedChannel.data.isWholeBuffer)
    assertEquals(rankedChannel(1, 2, 3), 95.0)

  test("time-series and component-axis views are checked and zero-copy"):
    val frame = geometryRight(Frame.named[D2]("semantic-axis-views"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1), Affine.identity[D2]))
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val direction =
      imageRight(Axis.create("direction", 3, AxisKind.Direction))
    val axes = imageRight(NonSpatialAxes.from(Vector(time, direction)))
    val data =
      NDArray.fromSeq(
        Shape(1, 1, 2, 3),
        Vector(0.0, 1.0, 2.0, 10.0, 11.0, 12.0)
      )
    val sampled =
      imageRight(Sampled.continuous(grid, axes, data))
    val timeSeries = imageRight(TimeSeriesView.from(sampled))
    val components =
      imageRight(ComponentAxisView.from(sampled, AxisKind.Direction))
    val timePoint = imageRight(timeSeries.at(1))
    val component = imageRight(components.at(2))

    assert(timeSeries.image eq sampled)
    assert(components.image eq sampled)
    assert(timeSeries.image.data eq data)
    assert(components.image.data eq data)
    assert(timeSeries.axis eq time)
    assert(components.axis eq direction)
    assert(!timePoint.data.isWholeBuffer)
    assert(!component.data.isWholeBuffer)
    assertEquals(timePoint.nonSpatialAxes.records, Vector(direction.record))
    assertEquals(component.nonSpatialAxes.records, Vector(time.record))

  test("non-spatial selection reports axis, kind, and coordinate failures"):
    val frame = geometryRight(Frame.named[D3]("axis-errors"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3])
      )
    val volume =
      imageRight(
        Sampled.continuous(
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
      TimeSeriesView.from(volume),
      Left(ImageError.MissingNonSpatialAxisKind(AxisKind.Time))
    )
    assertEquals(
      ComponentAxisView.from(volume, AxisKind.Direction),
      Left(ImageError.MissingNonSpatialAxisKind(AxisKind.Direction))
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
        Sampled.continuous(
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
      TimeSeriesView.from(ambiguous),
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

    val directionA =
      imageRight(Axis.create("direction-a", 2, AxisKind.Direction))
    val directionB =
      imageRight(Axis.create("direction-b", 2, AxisKind.Direction))
    val directionAxes =
      imageRight(NonSpatialAxes.from(Vector(directionA, directionB)))
    val ambiguousComponents =
      imageRight(
        Sampled.continuous(
          grid,
          directionAxes,
          NDArray.fromSeq(
            dynamicShape(1, 1, 1, 2, 2),
            0 until 4 map (_.toDouble)
          )
        )
      )
    assertEquals(
      ComponentAxisView.from(
        ambiguousComponents,
        AxisKind.Direction
      ),
      Left(
        ImageError.AmbiguousNonSpatialAxisKind(AxisKind.Direction, 2)
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
            0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0
          )
        )
      )
    val grid =
      geometryRight(Grid.in(frame)(Vector(5, 6, 7), affine))
    val source =
      imageRight(
        Sampled.continuous(
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
      geometryRight(
        grid.pointAt(geometryRight(LatticeIndex.of[D3](1, 2, 3)))
      )
    val viewOrigin =
      geometryRight(
        view.grid.pointAt(
          geometryRight(LatticeIndex.of[D3](0, 0, 0))
        )
      )

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
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, base))
    val reversed =
      imageRight(
        Sampled.continuous(grid, NonSpatialAxes.empty, base.reverse(0))
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
        Sampled.continuous(
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
          assert(d2.value.frame eq frame)
          d2.value.logicalShape
        ,
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
        Sampled.continuous(
          grid,
          axes,
          NDArray.zeros[Double, Rank[4]](Shape(2, 2, 2, 3))
        )
      )
    val discovered: SomeSampled[Double, Continuous] =
      SomeSampled.d3(sampled)

    val ranks =
      discovered.fold(
        _ => fail("expected D3"),
        d3 =>
          assert(d3.value eq sampled)
          d3.dimension.rank -> d3.value.data.shape.rank
      )

    assertEquals(ranks, 3 -> 4)

  test("SomeSampled lifts rank-preserving metadata, maps, and exact views"):
    val frame = geometryRight(Frame.named[D3]("dynamic-operations"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(3, 4, 2), Affine.identity[D3]))
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](3, 4, 2, 2) { (i, j, k, t) =>
            1000.0 * i + 100.0 * j + 10.0 * k + t
          }
        )
      )
    val discovered = SomeSampled.d3(sampled)

    assert(discovered.sampleSpace eq sampled.sampleSpace)
    assert(discovered.grid eq sampled.grid)
    assert(discovered.data eq sampled.data)
    assertEquals(discovered.logicalShape, Vector(3, 4, 2, 2))
    assertEquals(discovered.nonSpatialAxes.records, axes.records)
    assertEquals(discovered.valueAt(Vector(2, 3, 1), Vector(1)), Right(2311.0))

    val named = discovered.withMetadata(ImageMetadata.named("named"))
    assertEquals(named.metadata, ImageMetadata.named("named"))
    assert(named.data eq sampled.data)

    val centered = discovered.mapValues(_ - 1000.0)
    assertEquals(centered.valueAt(Vector(2, 3, 1), Vector(1)), Right(1311.0))
    assert(centered.sampleSpace eq sampled.sampleSpace)

    val mask: SomeSampled[Boolean, Mask] =
      discovered.mapValuesAs[Boolean, Mask](_ >= 1000.0)
    assertEquals(mask.valueAt(Vector(0, 0, 0), Vector(0)), Right(false))
    assertEquals(mask.valueAt(Vector(2, 0, 0), Vector(0)), Right(true))

    val crop = imageRight(
      discovered.crop(origin = Vector(1, 1, 0), shape = Vector(2, 2, 2))
    )
    assertEquals(crop.logicalShape, Vector(2, 2, 2, 2))
    crop.fold(
      _ => fail("expected D3 crop"),
      d3 =>
        assert(d3.value.frame eq frame)
        assert(!d3.value.data.isCanonicalLayout)
        assertEquals(d3.valueAt(Vector(0, 0, 0), Vector(1)), Right(1101.0))
    )

  test("SomeSampled hides initial drop evidence while preserving the lowered rank"):
    val frame = geometryRight(Frame.named[D3]("dynamic-selection"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3]))
    val time = imageRight(Axis.create("time", 3, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.zeros[Double, Rank[4]](Shape(2, 2, 2, 3))
        )
      )
    val discovered = SomeSampled.d3(sampled)

    val selected: Sampled[
      ? <: SampleSpace[discovered.F, D3],
      Double,
      Continuous,
      Rank[3]
    ] = imageRight(discovered.atTime(1))

    assertEquals(selected.logicalShape, Vector(2, 2, 2))
    assertEquals(selected.data.shape.rank, 3)
    assert(selected.frame eq frame)

    val dynamicSampled =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.zeros[Double, AnyRank](dynamicShape(2, 2, 2, 3))
        )
      )
    val dynamic = SomeSampled.d3(dynamicSampled)
    val dynamicSelected = imageRight(dynamic.atTime(2))

    assertEquals(dynamicSelected.logicalShape, Vector(2, 2, 2))
    assertEquals(dynamicSelected.data.shape.rank, 3)

  test("SomeSampled packaging requires honest drop-axis evidence"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.geometry.*
import ravel.AnyRank

def packageUnknown[
  F <: Frame[D3],
  S <: SampleSpace[F, D3],
  R <: AnyRank
](sampled: Sampled[S, Double, Continuous, R]) =
  SomeSampled.d3(sampled)
"""
    )

    assert(errors.exists(_.message.contains("CanDropAxis")))

  test("SomeSampled constructors reject the wrong spatial dimension"):
    val errors = typeCheckErrors(
      """
import image4s.*
import ravel.NDArray
import image4s.geometry.*
val frame = Frame.named[D3]("volume").toOption.get
val grid = Grid.in(frame)(Vector(1, 1, 1), Affine.identity[D3]).toOption.get
val sampled = Sampled
  .continuous(grid, NonSpatialAxes.empty, NDArray.zeros[Double](1, 1, 1))
  .toOption
  .get
SomeSampled.d2(sampled)
"""
    )

    assert(errors.nonEmpty)

    val extensionErrors = typeCheckErrors(
      """
import image4s.*
trait UnsupportedPackage extends SomeSampled[Double, Continuous]
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
  .continuous(grid, NonSpatialAxes.empty, NDArray.zeros[Double, AnyRank](dynamic))
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
  .continuous(grid, axes, NDArray.zeros[Double](1, 1, 1, 1))
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
      case Left(error) => fail(error.toString)

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
