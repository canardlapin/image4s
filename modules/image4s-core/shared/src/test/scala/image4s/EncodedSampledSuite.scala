package image4s

import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

final class EncodedSampledSuite extends FunSuite:
  test("identity encoding materializes the original dense storage"):
    val space = sampleSpace(Vector(2, 2))
    val data = NDArray.fromSeq(Shape(2, 2), Vector(0, 1, 2, 3))
    val encoded =
      imageRight(
        EncodedSampled.identity[Int, Categorical, Rank[2]](space, data)
      )

    val materialized = imageRight(encoded.materializeTo)

    assert(materialized.data eq data)
    assertEquals(materialized.data.elementsIterator.toList, List(0, 1, 2, 3))

  test("uniform affine decoding is lazy and materializes domain values"):
    val space = sampleSpace(Vector(2, 2))
    val encoding = encodingRight(ValueEncoding.UniformAffine.create(2.0, 1.0))
    val encoded =
      imageRight(
        EncodedSampled.create[Double, Double, Continuous, Rank[2]](
          space,
          NDArray.fromSeq(Shape(2, 2), Vector(0.0, 1.0, 2.0, 3.0)),
          encoding,
          ImageMetadata.named("scaled")
        )
      )

    assertEquals(encoded.valueAt(Vector(1, 0)), Right(5.0))
    assertEquals(encoded.fingerprint, encoding.fingerprint)

    val materialized = imageRight(encoded.materializeTo)
    assertEquals(materialized.data.elementsIterator.toList, List(1.0, 3.0, 5.0, 7.0))
    assertEquals(materialized.metadata, ImageMetadata.named("scaled"))

  test("uniform affine valueAt and materialization match the equation"):
    val space = sampleSpace(Vector(3, 2))
    val encoding = encodingRight(ValueEncoding.UniformAffine.create(-0.5, 3.25))
    val stored = Vector(-8.0, -1.0, 0.0, 2.0, 4.0, 16.0)
    val encoded =
      imageRight(
        EncodedSampled.create[Double, Double, Continuous, Rank[2]](
          space,
          NDArray.fromSeq(Shape(3, 2), stored),
          encoding
        )
      )

    val materialized = imageRight(encoded.materializeTo)
    for
      first <- 0 until 3
      second <- 0 until 2
    do
      val spatial = Vector(first, second)
      val value = imageRight(encoded.stored.valueAt(spatial))
      val expected = value * encoding.slope + encoding.intercept
      assertEquals(encoded.valueAt(spatial), Right(expected))
      assertEquals(imageRight(materialized.valueAt(spatial)), expected)

  test("per-axis affine encoding follows its non-spatial coordinate"):
    val frame = geometryRight(Frame.named[D2]("per-axis"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(1, 1), Affine.identity[D2]))
    val channel = imageRight(Axis.create("channel", 2, AxisKind.Channel))
    val space = SampleSpace.create(grid, imageRight(NonSpatialAxes.from(Vector(channel))))
    val encoding =
      encodingRight(
        ValueEncoding.PerAxisAffine.create(
          axis = 0,
          slopes = Vector(1.0, 10.0),
          intercepts = Vector(0.0, 1.0)
        )
      )
    val encoded =
      imageRight(
        EncodedSampled.create[Double, Double, Continuous, Rank[3]](
          space,
          NDArray.fromSeq(Shape(1, 1, 2), Vector(2.0, 2.0)),
          encoding
        )
      )

    assertEquals(encoded.valueAt(Vector(0, 0), Vector(0)), Right(2.0))
    assertEquals(encoded.valueAt(Vector(0, 0), Vector(1)), Right(21.0))
    assertEquals(
      imageRight(encoded.materializeTo).data.elementsIterator.toList,
      List(2.0, 21.0)
    )

  test("spatial crops preserve encoding and decoded alignment"):
    val frame = geometryRight(Frame.named[D2]("encoded-crop"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(3, 2), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val encoding = encodingRight(ValueEncoding.UniformAffine.create(3.0, -1.0))
    val encoded =
      imageRight(
        EncodedSampled.create[Double, Double, Continuous, Rank[2]](
          space,
          NDArray.fromSeq(Shape(3, 2), Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)),
          encoding
        )
      )

    val cropped = imageRight(encoded.crop(Vector(1, 0), Vector(2, 2)))

    assertEquals(cropped.fingerprint, encoded.fingerprint)
    assertEquals(cropped.valueAt(Vector(0, 1)), Right(8.0))
    assertEquals(cropped.valueAt(Vector(1, 0)), Right(11.0))

  test("codebook encodings reject invalid stored codes deterministically"):
    val first = encodingRight(ValueEncoding.Codebook.create(Vector("left", "right")))
    val second = encodingRight(ValueEncoding.Codebook.create(Vector("left", "right")))
    assertEquals(first, second)
    assertEquals(first.fingerprint, second.fingerprint)
    assertEquals(first.decode(1, Vector.empty), Right("right"))

  private def sampleSpace(shape: Vector[Int]): SampleSpace[?, ?] =
    val frame = geometryRight(Frame.named[D2]("encoded"))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    SampleSpace.create(grid, NonSpatialAxes.empty)

  private def encodingRight[A](value: Either[EncodingError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
