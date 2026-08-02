package image4s.intaglio

import _root_.intaglio.DisplayWindow
import _root_.intaglio.GridSampling
import _root_.intaglio.Rgba32
import _root_.intaglio.alpha
import _root_.intaglio.blue
import _root_.intaglio.green
import _root_.intaglio.red
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

final class DisplayBridgeSuite extends FunSuite:
  test("field lowering preserves axis coordinates and x-fastest values"):
    val image = continuous(Affine.identity[D2])

    val field = bridgeRight(DisplayBridge.toIntaglioField(image))

    assertEquals(field.xAxis.sampling, GridSampling.CellCentered)
    assertEquals(field.yAxis.sampling, GridSampling.CellCentered)
    assertEquals(field.xAxis.coordinate(0), Some(0.0))
    assertEquals(field.xAxis.coordinate(1), Some(1.0))
    assertEquals(field.yAxis.coordinate(0), Some(0.0))
    assertEquals(field.yAxis.coordinate(1), Some(1.0))
    assertEquals(field.samples, Vector(0.0, 2.0, 1.0, 3.0))

  test("field lowering reverses reflected display axes without changing samples"):
    val reflected =
      geometryRight(
        Affine.fromRowMajor[D2](
          Vector(
            -1.0,
            0.0,
            1.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            1.0
          )
        )
      )

    val field = bridgeRight(DisplayBridge.toIntaglioField(continuous(reflected)))

    assertEquals(field.xAxis.coordinate(0), Some(0.0))
    assertEquals(field.xAxis.coordinate(1), Some(1.0))
    assertEquals(field.samples, Vector(2.0, 0.0, 3.0, 1.0))

  test("field lowering rejects sheared grids rather than reinterpreting them"):
    val sheared =
      geometryRight(
        Affine.fromRowMajor[D2](
          Vector(
            1.0,
            0.25,
            0.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            1.0
          )
        )
      )

    DisplayBridge.toIntaglioField(continuous(sheared)) match
      case Left(_: DisplayBridgeError.NonAxisAlignedField) =>
        ()
      case other =>
        fail(s"expected a non-axis-aligned field error, got $other")

  test("raster lowering applies the palette and orientation in its pack loop"):
    val image = continuous(Affine.identity[D2])
    val plan =
      DisplayPlan(
        DisplayWindow.unsafe(0.0, 3.0),
        orientation = DisplayOrientation(transpose = true, flipX = true)
      )

    val raster = DisplayBridge.renderRaster(image, plan)

    assertEquals(raster.width, 2)
    assertEquals(raster.height, 2)
    assertEquals(pixelChannels(raster.pixel(0, 0)), (85, 85, 85, 255))
    assertEquals(pixelChannels(raster.pixel(1, 0)), (0, 0, 0, 255))
    assertEquals(pixelChannels(raster.pixel(0, 1)), (255, 255, 255, 255))
    assertEquals(pixelChannels(raster.pixel(1, 1)), (170, 170, 170, 255))

  private def continuous(
      affine: Affine[D2]
  ) =
    val frame = geometryRight(Frame.named[D2]("display-bridge"))
    val grid = geometryRight(Grid.in(frame)(Vector(2, 2), affine))
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(Shape(2, 2), Vector(0.0, 1.0, 2.0, 3.0))
      )
    )

  private def pixelChannels(
      pixel: Either[_root_.intaglio.GraphicsError, Rgba32]
  ): (Int, Int, Int, Int) =
    pixel match
      case Right(value) =>
        (value.red, value.green, value.blue, value.alpha)
      case Left(error) =>
        fail(error.message)

  private def bridgeRight[A](
      value: Either[DisplayBridgeError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

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
