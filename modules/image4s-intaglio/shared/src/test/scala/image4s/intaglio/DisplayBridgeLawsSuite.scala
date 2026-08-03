package image4s.intaglio

import _root_.intaglio.DisplayOpacity
import _root_.intaglio.DisplayWindow
import _root_.intaglio.Rgba32
import _root_.intaglio.alpha
import _root_.intaglio.blue
import _root_.intaglio.green
import _root_.intaglio.red
import _root_.intaglio.toPackedInt
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
import ravel.Rank
import ravel.Shape

/** Display conformance laws: exact grayscale endpoints, byte-exact alpha, RGBA packing round-trips,
  * and single orientation application.
  */
final class DisplayBridgeLawsSuite extends FunSuite:
  test("grayscale endpoints are exact and out-of-window values clamp to them"):
    val image =
      continuous(
        Vector(4, 1),
        Vector(-10.0, 0.0, 3.0, 40.0)
      )
    val raster =
      DisplayBridge.renderRaster(
        image,
        DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
      )

    assertEquals(channels(raster.pixel(0, 0)), (0, 0, 0, 255))
    assertEquals(channels(raster.pixel(1, 0)), (0, 0, 0, 255))
    assertEquals(channels(raster.pixel(2, 0)), (255, 255, 255, 255))
    assertEquals(channels(raster.pixel(3, 0)), (255, 255, 255, 255))

  test("overlay alpha is byte-exact at the opaque and transparent extremes"):
    val image = continuous(Vector(2, 2), Vector(0.0, 1.0, 2.0, 3.0))
    val mask =
      imageRight(
        Sampled.mask(
          image.sampleSpace,
          NDArray.fromSeq(Shape(2, 2), Vector(true, true, false, false))
        )
      )
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
    val base = DisplayBridge.renderRaster(image, plan)
    val opaque =
      bridgeRight(
        DisplayBridge.renderRasterWithMask(
          image,
          mask,
          plan,
          MaskOverlay(Rgba32.unsafe(12, 34, 56))
        )
      )
    val transparent =
      bridgeRight(
        DisplayBridge.renderRasterWithMask(
          image,
          mask,
          plan,
          MaskOverlay(Rgba32.unsafe(12, 34, 56), DisplayOpacity.Transparent)
        )
      )

    assertEquals(channels(opaque.pixel(0, 0)), (12, 34, 56, 255))
    assertEquals(channels(opaque.pixel(0, 1)), (12, 34, 56, 255))
    assertEquals(channels(opaque.pixel(1, 0)), channels(base.pixel(1, 0)))
    var y = 0
    while y < 2 do
      var x = 0
      while x < 2 do
        assertEquals(
          channels(transparent.pixel(x, y)),
          channels(base.pixel(x, y)),
          "a fully transparent overlay must leave every base byte unchanged"
        )
        x += 1
      y += 1

  test("RGBA channels round-trip through packed words and raster pixels"):
    val samples =
      Vector(
        (0, 0, 0, 0),
        (255, 255, 255, 255),
        (12, 34, 56, 78),
        (1, 254, 127, 128)
      )
    samples.foreach { (red0, green0, blue0, alpha0) =>
      val pixel = Rgba32.unsafe(red0, green0, blue0, alpha0)
      assertEquals((pixel.red, pixel.green, pixel.blue, pixel.alpha), (red0, green0, blue0, alpha0))
      assertEquals(
        pixel.toPackedInt,
        (red0 << 24) | (green0 << 16) | (blue0 << 8) | alpha0
      )
    }

    val labels =
      imageRight(
        Sampled.categorical[Int, Rank[2]](
          continuous(Vector(2, 2), Vector(0.0, 0.0, 0.0, 0.0)).sampleSpace,
          NDArray.fromSeq(Shape(2, 2), Vector(0, 5, 9, 13))
        )
      )
    val palette = LabelPalette()
    val raster = DisplayBridge.renderLabels(labels, palette)
    var linear = 0
    while linear < 4 do
      val expected = palette.color(labels.data(linear / 2, linear % 2))
      raster.pixel(linear / 2, linear % 2) match
        case Right(actual) =>
          assertEquals(actual.toPackedInt, expected.toPackedInt)
        case Left(error) => fail(error.message)
      linear += 1

  test("orientation is applied exactly once during packing"):
    val image =
      continuous(
        Vector(3, 2),
        Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)
      )
    val window = DisplayWindow.unsafe(0.0, 5.0)
    val identity = DisplayBridge.renderRaster(image, DisplayPlan(window))
    val orientation = DisplayOrientation(transpose = true, flipX = true, flipY = true)
    val oriented =
      DisplayBridge.renderRaster(
        image,
        DisplayPlan(window, orientation = orientation)
      )

    assertEquals(oriented.width, 2)
    assertEquals(oriented.height, 3)
    var y = 0
    while y < oriented.height do
      var x = 0
      while x < oriented.width do
        val orientedX = oriented.width - 1 - x
        val orientedY = oriented.height - 1 - y
        val sourceX = orientedY
        val sourceY = orientedX
        assertEquals(
          channels(oriented.pixel(x, y)),
          channels(identity.pixel(sourceX, sourceY)),
          s"pixel ($x, $y) must map through one orientation resolution"
        )
        x += 1
      y += 1

  private def continuous(shape: Vector[Int], values: Vector[Double]) =
    val frame = geometryRight(Frame.named[D2]("display-laws"))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def channels(
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
      case Left(error) => fail(error.message)

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
