package image4s.reference

import image4s.Axis
import image4s.AxisKind
import image4s.BoundaryPolicy
import image4s.ImageError
import image4s.LinearInterpolable
import image4s.LinearSampling
import image4s.NonSpatialAxes
import image4s.PartialWeight.value
import image4s.Sampled
import image4s.Validity
import image4s.ValueSemantics
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.FrameId
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Point
import scala.compiletime.testing.typeCheckErrors

final class ReferenceSamplerSuite extends FunSuite:
  test("linear reference sampling reproduces a D2 affine scalar field"):
    val frame = geometryRight(Frame.named[D2]("affine-field"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(3, 3), Affine.identity[D2]))
    val values =
      for
        i <- 0 until 3
        j <- 0 until 3
      yield 2.0 * i.toDouble + 3.0 * j.toDouble + 1.0
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(3, 3), values)
        )
      )
    val point = geometryRight(Point.in(frame)(0.25, 1.5))
    val result = imageRight(ReferenceSampler.linear(sampled, point))

    assertEqualsDouble(result.value, 6.0, 1e-12)
    assertEquals(result.validity, Validity.Full)

  test("linear reference sampling reproduces a D3 affine scalar field"):
    val frame = geometryRight(Frame.named[D3]("affine-field-3d"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(3, 3, 3), Affine.identity[D3]))
    val values =
      for
        i <- 0 until 3
        j <- 0 until 3
        k <- 0 until 3
      yield i.toDouble + 2.0 * j.toDouble - k.toDouble + 4.0
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(3, 3, 3), values)
        )
      )
    val point = geometryRight(Point.in(frame)(0.25, 1.5, 0.5))
    val result = imageRight(ReferenceSampler.linear(sampled, point))

    assertEqualsDouble(result.value, 6.75, 1e-12)
    assertEquals(result.validity, Validity.Full)

  test("nearest reference sampling supports labels"):
    val frame = geometryRight(Frame.named[D2]("labels"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1, 2, 3, 4))
        )
      )
    val point = geometryRight(Point.in(frame)(0.8, 0.1))
    val result = imageRight(ReferenceSampler.nearest(sampled, point))

    assertEquals(result.value, 3)
    assertEquals(result.validity, Validity.Full)

  test("reference sampling indexes explicit time and channel axes"):
    val frame = geometryRight(Frame.named[D2]("extra-axes"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1), Affine.identity[D2]))
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val channel =
      imageRight(Axis.create("channel", 2, AxisKind.Channel))
    val axes =
      imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.fromSeq(
            Shape(1, 1, 2, 2),
            Vector(10.0, 11.0, 20.0, 21.0)
          )
        )
      )
    val point = geometryRight(Point.in(frame)(0.0, 0.0))
    val result =
      imageRight(
        ReferenceSampler.nearest(
          sampled,
          point,
          nonSpatialIndex = Vector(1, 0)
        )
      )

    assertEquals(result.value, 20.0)
    assertEquals(result.validity, Validity.Full)

  test("label data is rejected by the linear API at compile time"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.reference.ReferenceSampler
import ravel.AnyRank
import image4s.geometry.*
def invalid[F <: Frame[D2], S <: SampleSpace[F, D2]](
  image: Sampled[S, Double, Categorical, AnyRank],
  point: Point[F, D2]
): Unit =
  ReferenceSampler.linear(image, point)
"""
    )
    assert(errors.nonEmpty)

  test("downstream semantics may opt into linear reference sampling"):
    sealed trait Probability
    given ValueSemantics[Double, Probability] with {}
    given LinearSampling[Double, Probability] with
      val interpolation: LinearInterpolable[Double] =
        summon[LinearInterpolable[Double]]

    val frame = geometryRight(Frame.named[D2]("probability"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.create[
          frame.type,
          D2,
          Double,
          Probability,
          Rank[2]
        ](
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(
            Shape(2, 2),
            Vector(0.0, 0.5, 0.5, 1.0)
          )
        )
      )
    val point = geometryRight(Point.in(frame)(0.5, 0.5))
    val result = imageRight(ReferenceSampler.linear(sampled, point))

    assertEqualsDouble(result.value, 0.5, 1e-12)
    assertEquals(result.validity, Validity.Full)

  test("constant boundaries report partial support and outside support"):
    val frame = geometryRight(Frame.named[D2]("boundary"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(
            Shape(2, 2),
            Vector(1.0, 1.0, 1.0, 1.0)
          )
        )
      )
    val partialPoint = geometryRight(Point.in(frame)(-0.25, 0.5))
    val partial =
      imageRight(
        ReferenceSampler.linear(
          sampled,
          partialPoint,
          boundary = BoundaryPolicy.Constant(0.0)
        )
      )
    partial.validity match
      case Validity.Partial(weight) =>
        assertEqualsDouble(weight.value, 0.75, 1e-12)
      case other =>
        fail(s"expected partial validity, got $other")

    val outsidePoint = geometryRight(Point.in(frame)(-2.0, -2.0))
    val outside =
      imageRight(
        ReferenceSampler.linear(
          sampled,
          outsidePoint,
          boundary = BoundaryPolicy.Constant(0.0)
        )
      )
    assertEquals(outside.value, 0.0)
    assertEquals(outside.validity, Validity.Outside)

  test("reject boundaries return OutsideGrid rather than a sentinel"):
    val frame = geometryRight(Frame.named[D2]("reject-boundary"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](2, 2)
        )
      )
    val point = geometryRight(Point.in(frame)(-0.25, 0.5))

    ReferenceSampler.linear(sampled, point) match
      case Left(ImageError.OutsideGrid(index)) =>
        assertEquals(index, Vector(-0.25, 0.5))
      case other =>
        fail(s"expected OutsideGrid, got $other")

  test("checked reference boundary reports distinct frame ownership"):
    val imageFrame = geometryRight(Frame.named[D2]("image-frame"))
    val pointFrame = geometryRight(Frame.named[D2]("point-frame"))
    val grid =
      geometryRight(
        Grid.in(imageFrame)(Vector(2, 2), Affine.identity[D2])
      )
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](2, 2)
        )
      )
    val point = geometryRight(Point.in(pointFrame)(0.0, 0.0))

    ReferenceSampler.nearestChecked(sampled, point) match
      case Left(
            ImageError.Geometry(GeometryError.EphemeralFrameMismatch)
          ) =>
        assert(true)
      case other =>
        fail(s"expected a typed frame mismatch, got $other")

  test("checked reference boundary rebinds matching restored frame identities"):
    val id = geometryRight(FrameId.parse("frame-reference-restored"))
    val original =
      geometryRight(
        Frame.persistentNamed[D2](id, "restored-frame")
      )
    val record = geometryRight(original.record)
    val left =
      geometryRight(
        Frame.restore[D2](record, Frame.Registry.empty)
      ).frame
    val right =
      geometryRight(
        Frame.restore[D2](record, Frame.Registry.empty)
      ).frame
    assert(left ne right)
    val grid =
      geometryRight(Grid.in(left)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(
            Shape(2, 2),
            Vector(3.0, 4.0, 5.0, 6.0)
          )
        )
      )
    val point = geometryRight(Point.in(right)(1.0, 0.0))
    val result =
      imageRight(ReferenceSampler.nearestChecked(sampled, point))

    assertEquals(result.value, 5.0)
    assertEquals(result.validity, Validity.Full)

  test("the convenience reference boundary preserves static frame ownership"):
    val errors = typeCheckErrors(
      """
import image4s.*
import image4s.reference.ReferenceSampler
import ravel.DType.given
import ravel.NDArray
import image4s.geometry.*
val imageFrame = Frame.named[D2]("image").toOption.get
val pointFrame = Frame.named[D2]("point").toOption.get
val grid = Grid.in(imageFrame)(Vector(2, 2), Affine.identity[D2]).toOption.get
val image = Sampled.continuous(
  grid,
  NonSpatialAxes.empty,
  NDArray.zeros[Double](2, 2)
).toOption.get
val point = Point.in(pointFrame)(0.0, 0.0).toOption.get
ReferenceSampler.nearest(image, point)
"""
    )
    assert(errors.nonEmpty)

  test("reference API exposes neither production plans nor a generic Sampler"):
    val missingPlan = typeCheckErrors(
      """
import image4s.reference.*
val plan = ResamplingPlan
"""
    )
    val missingGenericSampler = typeCheckErrors(
      """
import image4s.reference.*
val sampler = Sampler
"""
    )
    val missingCompile = typeCheckErrors(
      """
import image4s.reference.ReferenceSampler
val compiled = ReferenceSampler.compile
"""
    )

    assert(missingPlan.nonEmpty)
    assert(missingGenericSampler.nonEmpty)
    assert(missingCompile.nonEmpty)

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
