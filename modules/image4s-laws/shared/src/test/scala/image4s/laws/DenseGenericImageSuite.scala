package image4s.laws

import image4s.Axis
import image4s.AxisKind
import image4s.CategoricalImage
import image4s.ContinuousImage
import image4s.ImageError
import image4s.ImageMetadata
import image4s.MaskImage
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.apply
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

/** Laws proving dense aliases stay representation-generic and zero-copy. */
final class DenseGenericImageSuite extends FunSuite:
  test("continuous Float/Double construction preserves the Ravel array"):
    checkContinuousPreservation(
      NDArray.fromSeq(Shape(2, 3), Vector(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f))
    )
    checkContinuousPreservation(
      NDArray.fromSeq(Shape(2, 3), Vector(1.0, 2.0, 3.0, 4.0, 5.0, 6.0))
    )

  test("categorical integral construction preserves the Ravel array"):
    checkCategoricalPreservation(
      NDArray.fromSeq(Shape(2, 2), Vector[Byte](1, 2, 3, 4))
    )
    checkCategoricalPreservation(
      NDArray.fromSeq(Shape(2, 2), Vector[Short](1, 2, 3, 4))
    )
    checkCategoricalPreservation(
      NDArray.fromSeq(Shape(2, 2), Vector(1, 2, 3, 4))
    )
    checkCategoricalPreservation(
      NDArray.fromSeq(Shape(2, 2), Vector(1L, 2L, 3L, 4L))
    )

  test("mask construction preserves Boolean storage"):
    val frame = geometryRight(Frame.named[D2]("mask-preserve"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val data =
      NDArray.fromSeq(Shape(2, 2), Vector(true, false, true, false))
    val mask = imageRight(Sampled.mask(grid, NonSpatialAxes.empty, data))
    val asAlias: MaskImage[? <: SampleSpace[?, ?], Rank[2]] = mask
    assert(mask.data eq data)
    assert(asAlias eq mask)
    assertEquals(mask.dtype.name, "Boolean")
    assertEquals(mask(0, 1), false)

  test("shape validation is independent of dtype"):
    val frame = geometryRight(Frame.named[D2]("shape-mismatch"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val badFloat =
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.zeros[Float](2, 3)
      )
    val badInt =
      Sampled.categorical(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(Shape(2, 3), Vector(0, 1, 2, 3, 4, 5))
      )
    (badFloat, badInt) match
      case (
            Left(ImageError.SampledShapeMismatch(expectedF, actualF)),
            Left(ImageError.SampledShapeMismatch(expectedI, actualI))
          ) =>
        assertEquals(expectedF, Vector(2, 2))
        assertEquals(expectedI, Vector(2, 2))
        assertEquals(actualF, Vector(2, 3))
        assertEquals(actualI, Vector(2, 3))
      case other =>
        fail(s"expected matched shape mismatches, got $other")

  test("crop and axis selection remain views across dtypes"):
    val frame = geometryRight(Frame.named[D3]("views"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(4, 3, 2), Affine.identity[D3]))
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val floats =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Float](4, 3, 2, 2)((i, j, k, t) =>
            (1000 * i + 100 * j + 10 * k + t).toFloat
          )
        )
      )
    val labels =
      imageRight(
        Sampled.categorical(
          grid,
          axes,
          NDArray.tabulate[Int](4, 3, 2, 2)((i, j, k, t) =>
            1000 * i + 100 * j + 10 * k + t
          )
        )
      )

    val floatCrop = imageRight(floats.crop(Vector(1, 1, 0), Vector(2, 2, 2)))
    val labelCrop = imageRight(labels.crop(Vector(1, 1, 0), Vector(2, 2, 2)))
    assert(!floatCrop.data.isCanonicalLayout)
    assert(!labelCrop.data.isCanonicalLayout)
    assertEquals(floatCrop.dtype.name, "Float")
    assertEquals(labelCrop.dtype.name, "Int")
    assertEquals(floatCrop(0, 0, 0, 1), floats(1, 1, 0, 1))
    assertEquals(labelCrop(0, 0, 0, 1), labels(1, 1, 0, 1))

    val floatTime = imageRight(floats.selectNonSpatial(0, 1))
    val labelTime = imageRight(labels.selectNonSpatial(0, 1))
    assertEquals(floatTime.logicalShape, Vector(4, 3, 2))
    assertEquals(labelTime.logicalShape, Vector(4, 3, 2))
    assertEquals(floatTime(1, 1, 0), floats(1, 1, 0, 1))
    assertEquals(labelTime(1, 1, 0), labels(1, 1, 0, 1))

  test("canonical and strided layouts agree logically for Float and Int"):
    val frame = geometryRight(Frame.named[D2]("layout"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(3, 2), Affine.identity[D2]))
    val canonical =
      NDArray.tabulate[Float](3, 2)((i, j) => (10 * i + j).toFloat)
    val strided = canonical.reverse(0)
    val left =
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, canonical))
    val right =
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, strided))
    assertEquals(left(0, 1), right(2, 1))
    assertEquals(left(2, 0), right(0, 0))

    val labels =
      NDArray.tabulate[Int](3, 2)((i, j) => 10 * i + j)
    val reversedLabels = labels.reverse(0)
    val labelLeft =
      imageRight(Sampled.categorical(grid, NonSpatialAxes.empty, labels))
    val labelRight =
      imageRight(
        Sampled.categorical(grid, NonSpatialAxes.empty, reversedLabels)
      )
    assertEquals(labelLeft(0, 1), labelRight(2, 1))
    assertEquals(labelLeft(2, 0), labelRight(0, 0))

  test("metadata replacement preserves dtype and does not copy samples"):
    val frame = geometryRight(Frame.named[D2]("metadata"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val data = NDArray.zeros[Double](2, 2)
    val sampled =
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, data))
    val next =
      sampled.withMetadata(ImageMetadata.empty)
    val renamed =
      sampled.withMetadata(ImageMetadata.named("dense-generic"))
    assert(next eq sampled)
    assert(renamed.data eq data)
    assertEquals(renamed.dtype.name, "Double")
    assertEquals(renamed.metadata.label, "dense-generic")

  test("ranked access returns the exact primitive value for each dtype"):
    val frame = geometryRight(Frame.named[D2]("ranked"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val bytes =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector[Byte](7, 8, 9, 10))
        )
      )
    val shorts =
      imageRight(
        Sampled.categorical(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector[Short](7, 8, 9, 10))
        )
      )
    val floats =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1.25f, 2.5f, 3.75f, 5.0f))
        )
      )
    val doubles =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.fromSeq(Shape(2, 2), Vector(1.25, 2.5, 3.75, 5.0))
        )
      )
    assertEquals(bytes(1, 0), 9.toByte)
    assertEquals(shorts(1, 0), 9.toShort)
    assertEquals(floats(1, 0), 3.75f)
    assertEquals(doubles(1, 0), 3.75)

  private def checkContinuousPreservation[A](
      data: NDArray[A, Rank[2]]
  )(using image4s.ValueSemantics[A, image4s.Continuous]): Unit =
    val frame = geometryRight(Frame.named[D2](s"continuous-${data.dtype.name}"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3), Affine.identity[D2]))
    val sampled =
      imageRight(Sampled.continuous(grid, NonSpatialAxes.empty, data))
    val asAlias: ContinuousImage[? <: SampleSpace[?, ?], A, Rank[2]] =
      sampled
    assert(sampled.data eq data)
    assert(asAlias eq sampled)
    assertEquals(sampled.dtype.name, data.dtype.name)

  private def checkCategoricalPreservation[A](
      data: NDArray[A, Rank[2]]
  )(using image4s.ValueSemantics[A, image4s.Categorical]): Unit =
    val frame =
      geometryRight(Frame.named[D2](s"categorical-${data.dtype.name}"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val sampled =
      imageRight(Sampled.categorical(grid, NonSpatialAxes.empty, data))
    val asAlias: CategoricalImage[? <: SampleSpace[?, ?], A, Rank[2]] =
      sampled
    assert(sampled.data eq data)
    assert(asAlias eq sampled)
    assertEquals(sampled.dtype.name, data.dtype.name)

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
