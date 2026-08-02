package image4s.intaglio

import _root_.intaglio.ColorRamp
import _root_.intaglio.DisplayWindow
import _root_.intaglio.Rgba32
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape

final class DisplayBridgeCacheSuite extends FunSuite:
  test("repeated renders with one plan return the identical raster"):
    val cache = DisplayBridgeCache()
    val image = continuous()
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
    val first = cache.renderRaster(image, plan)
    val second = cache.renderRaster(image, plan)
    val third = cache.renderRaster(image, plan.copy())

    assert(first eq second, "an identical plan must hit the cache")
    assert(first eq third, "plan fingerprints are structural, not by instance")
    assertEquals(cache.entryCount, 1)

  test("any appearance change or new source instance misses the cache"):
    val cache = DisplayBridgeCache()
    val image = continuous()
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
    val base = cache.renderRaster(image, plan)

    val widened =
      cache.renderRaster(image, DisplayPlan(DisplayWindow.unsafe(0.0, 4.0)))
    val recolored =
      cache.renderRaster(image, plan.copy(palette = ColorRamp.Heat))
    val reoriented =
      cache.renderRaster(
        image,
        plan.copy(orientation = DisplayOrientation(flipX = true))
      )
    val rebuiltSource = cache.renderRaster(continuous(), plan)

    assert(!(base eq widened))
    assert(!(base eq recolored))
    assert(!(base eq reoriented))
    assert(!(base eq rebuiltSource), "identity keys never alias fresh sources")
    assertEquals(cache.entryCount, 5)

  test("mask, label, and slice renders cache with their appearance keys"):
    val cache = DisplayBridgeCache()
    val image = continuous()
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
    val mask =
      imageRight(
        Sampled.mask(
          image.sampleSpace,
          NDArray.fromSeq(Shape(2, 2), Vector(false, true, false, false))
        )
      )
    val overlay = MaskOverlay(Rgba32.unsafe(255, 0, 0))
    val maskedFirst =
      bridgeRight(cache.renderRasterWithMask(image, mask, plan, overlay))
    val maskedSecond =
      bridgeRight(cache.renderRasterWithMask(image, mask, plan, overlay))
    val maskedRecolored =
      bridgeRight(
        cache.renderRasterWithMask(
          image,
          mask,
          plan,
          MaskOverlay(Rgba32.unsafe(0, 255, 0))
        )
      )

    assert(maskedFirst eq maskedSecond)
    assert(!(maskedFirst eq maskedRecolored))

    val labels =
      imageRight(
        Sampled.categorical[Int, Rank[2]](
          image.sampleSpace,
          NDArray.fromSeq(Shape(2, 2), Vector(0, 7, 7, 11))
        )
      )
    val labelsFirst = cache.renderLabels(labels)
    val labelsSecond = cache.renderLabels(labels)
    val labelsFlipped =
      cache.renderLabels(labels, orientation = DisplayOrientation(flipY = true))

    assert(labelsFirst eq labelsSecond)
    assert(!(labelsFirst eq labelsFlipped))

    val volume = continuousD3()
    val sliceFirst =
      bridgeRight(cache.renderSliceRaster(volume, SliceAxis.Z, 0, plan))
    val sliceSecond =
      bridgeRight(cache.renderSliceRaster(volume, SliceAxis.Z, 0, plan))
    val sliceOther =
      bridgeRight(cache.renderSliceRaster(volume, SliceAxis.Z, 1, plan))

    assert(sliceFirst eq sliceSecond)
    assert(!(sliceFirst eq sliceOther))

  test("least-recently-used entries are evicted at the budget"):
    val cache = DisplayBridgeCache(maxEntries = 1)
    val image = continuous()
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))
    val other = DisplayPlan(DisplayWindow.unsafe(0.0, 4.0))

    val first = cache.renderRaster(image, plan)
    val displaced = cache.renderRaster(image, other)
    val recomputed = cache.renderRaster(image, plan)

    assertEquals(cache.entryCount, 1)
    assert(!(first eq recomputed), "the evicted entry must be recomputed")
    assert(displaced ne null)

  test("failed lowering is reported and never cached"):
    val cache = DisplayBridgeCache()
    val volume = continuousD3()
    val plan = DisplayPlan(DisplayWindow.unsafe(0.0, 3.0))

    assert(cache.renderSliceRaster(volume, SliceAxis.Z, 9, plan).isLeft)
    assertEquals(cache.entryCount, 0)

  private def continuous() =
    val frame = geometryRight(Frame.named[D2]("display-cache"))
    val grid = geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    imageRight(
      Sampled.continuous(
        grid,
        NonSpatialAxes.empty,
        NDArray.fromSeq(Shape(2, 2), Vector(0.0, 1.0, 2.0, 3.0))
      )
    )

  private def continuousD3() =
    val frame = geometryRight(Frame.named[D3]("display-cache-d3"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Double, Rank[3]](
        space,
        NDArray.tabulate[Double](2, 2, 2)((x, y, z) =>
          x.toDouble + 2.0 * y.toDouble + 4.0 * z.toDouble
        )
      )
    )

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
