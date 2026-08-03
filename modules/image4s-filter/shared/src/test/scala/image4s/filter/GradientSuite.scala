package image4s.filter

import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.ops.FrameCoordinates
import image4s.ops.IndexCoordinates
import image4s.ops.OpError
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

final class GradientSuite extends FunSuite:
  test("Sobel and Scharr reproduce an index-space affine field"):
    val image =
      float2D(Affine.identity[D2])((row, column) => row.toFloat + 2.0f * column.toFloat)
    val sobel = opsRight(image.sobel(IndexCoordinates))
    val scharr = opsRight(image.scharr(IndexCoordinates))

    Vector(sobel, scharr).foreach { field =>
      assertEquals(field.components.size, 2)
      assertEquals(field.domain, IndexCoordinates)
      assertEqualsDouble(field.components(0).data(3, 3).toDouble, 1.0, 1.0e-6)
      assertEqualsDouble(field.components(1).data(3, 3).toDouble, 2.0, 1.0e-6)
    }

  test("frame gradients apply the inverse-transpose on anisotropic grids"):
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(0.0, 0.0),
          spacing = Vector(2.0, 4.0),
          directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
        )
      )
    val image =
      float2D(affine)((row, column) => row.toFloat + 2.0f * column.toFloat)
    val index = opsRight(image.gradient(IndexCoordinates))
    val frame = opsRight(image.gradient(FrameCoordinates))

    assertEqualsDouble(index.components(0).data(3, 3).toDouble, 1.0, 1.0e-6)
    assertEqualsDouble(index.components(1).data(3, 3).toDouble, 2.0, 1.0e-6)
    assertEqualsDouble(frame.components(0).data(3, 3).toDouble, 0.5, 1.0e-6)
    assertEqualsDouble(frame.components(1).data(3, 3).toDouble, 0.5, 1.0e-6)

  test("frame gradients apply the full affine transform on rotated grids"):
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(0.0, 0.0),
          spacing = Vector(2.0, 4.0),
          directionRowMajor = Vector(0.0, -1.0, 1.0, 0.0)
        )
      )
    val image =
      float2D(affine)((row, column) => row.toFloat + 2.0f * column.toFloat)
    val frame = opsRight(image.gradient(FrameCoordinates))

    assertEqualsDouble(frame.components(0).data(3, 3).toDouble, -0.5, 1.0e-6)
    assertEqualsDouble(frame.components(1).data(3, 3).toDouble, 0.5, 1.0e-6)

  test("integral input requires an explicit floating gradient output"):
    val frame = geometryRight(Frame.named[D2]("byte-gradient"))
    val grid = geometryRight(Grid.in(frame)(Vector(7, 7), Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val image =
      imageRight(
        Sampled.continuous[Byte, Rank[2]](
          space,
          NDArray.tabulate[Byte](7, 7)((row, column) => (row + 2 * column).toByte)
        )
      )
    val output = opsRight(image.gradientTo[Float](IndexCoordinates))

    assertEqualsDouble(output.components(0).data(3, 3).toDouble, 1.0, 1.0e-5)
    assertEqualsDouble(output.components(1).data(3, 3).toDouble, 2.0, 1.0e-5)

  test("Sobel produces one normalized component per D3 axis"):
    val frame = geometryRight(Frame.named[D3]("d3-gradient"))
    val grid = geometryRight(Grid.in(frame)(Vector(7, 7, 7), Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[3]](
          space,
          NDArray.tabulate[Double](7, 7, 7)((x, y, z) =>
            x.toDouble + 2.0 * y.toDouble + 3.0 * z.toDouble
          )
        )
      )
    val field = opsRight(image.gradient(IndexCoordinates))

    assertEquals(field.components.size, 3)
    assertEqualsDouble(field.components(0).data(3, 3, 3), 1.0, 1.0e-12)
    assertEqualsDouble(field.components(1).data(3, 3, 3), 2.0, 1.0e-12)
    assertEqualsDouble(field.components(2).data(3, 3, 3), 3.0, 1.0e-12)

  private def float2D(
      affine: Affine[D2]
  )(
      values: (Int, Int) => Float
  ) =
    val frame = geometryRight(Frame.named[D2]("gradient-plane"))
    val grid = geometryRight(Grid.in(frame)(Vector(7, 7), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Float, Rank[2]](
        space,
        NDArray.tabulate[Float](7, 7)(values)
      )
    )

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)

  private def imageRight[A](value: Either[image4s.ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
