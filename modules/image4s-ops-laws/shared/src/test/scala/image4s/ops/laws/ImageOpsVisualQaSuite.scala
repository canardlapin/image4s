package image4s.ops.laws

import _root_.intaglio.RasterImage
import _root_.intaglio.alpha
import _root_.intaglio.red
import image4s.ops.laws.ImageOpsVisualQaFixtures.VisualCase
import munit.FunSuite

/** Cross-platform checks for the operation gallery's rendered evidence.
  *
  * These are intentionally small visual contracts rather than golden images: binary silhouettes
  * catch topology and orientation regressions, asymmetric filters catch correlation/convolution
  * reversals, and D3 slice dimensions catch axis-order mistakes. The JVM gallery turns the same
  * cases into artifacts for human inspection.
  */
final class ImageOpsVisualQaSuite extends FunSuite:
  private val cases: Vector[VisualCase] = ImageOpsVisualQaFixtures.build()
  private val byName: Map[String, RasterImage] =
    cases.map(example => example.name -> example.raster).toMap

  test("operation gallery contains distinct, deterministic raster stages"):
    assertEquals(
      cases.map(_.name),
      Vector(
        "d2-source",
        "d2-gaussian",
        "d2-sobel-x",
        "d2-sobel-y",
        "d2-scharr-x",
        "d2-scharr-y",
        "d2-correlation-asymmetric",
        "d2-convolution-asymmetric",
        "d2-gaussian-valid",
        "d2-gaussian-full",
        "d2-threshold",
        "d2-erode",
        "d2-dilate",
        "d2-opening",
        "d2-closing",
        "d2-box-dilate",
        "d2-disk-dilate",
        "d2-gaussian-impulse",
        "d3-source-x",
        "d3-source-y",
        "d3-source-z",
        "d3-gaussian-z",
        "d3-sobel-z",
        "d3-scharr-z",
        "d3-threshold-z",
        "d3-ball-dilate-z",
        "d3-ball-close-z"
      )
    )
    assertEquals(
      cases.map(example => (example.raster.width, example.raster.height)),
      Vector(
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (9, 9),
        (9, 9),
        (3, 3),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (15, 15),
        (9, 9),
        (6, 5),
        (7, 5),
        (7, 6),
        (7, 6),
        (7, 6),
        (7, 6),
        (7, 6),
        (7, 6),
        (7, 6)
      )
    )
    assertEquals(
      ImageOpsVisualQaFixtures.build().map(_.raster),
      cases.map(_.raster)
    )

  test("threshold, opening, and closing retain readable binary silhouettes"):
    assertEquals(
      maskAscii(byName("d2-threshold")),
      Vector(
        "...............",
        "...............",
        "...............",
        "...............",
        "....#######....",
        "....#######....",
        "....#######....",
        "....###.###....",
        "....#######....",
        "....#######....",
        "....#######....",
        "...............",
        "..#............",
        "...............",
        "..............."
      )
    )
    assertEquals(
      maskAscii(byName("d2-opening")),
      Vector(
        "...............",
        "...............",
        "...............",
        "...............",
        ".....#####.....",
        "....#######....",
        "....#######....",
        "....###.###....",
        "....#######....",
        "....#######....",
        ".....#####.....",
        "...............",
        "...............",
        "...............",
        "..............."
      )
    )
    assertEquals(
      maskAscii(byName("d2-closing")),
      Vector(
        "...............",
        "...............",
        "...............",
        "...............",
        "....#######....",
        "....#######....",
        "....#######....",
        "....#######....",
        "....#######....",
        "....#######....",
        "....#######....",
        "...............",
        "..#............",
        "...............",
        "..............."
      )
    )

  test("primitive and shaped morphology produce distinct, ordered silhouettes"):
    val threshold = foregroundCount(byName("d2-threshold"))
    val eroded = foregroundCount(byName("d2-erode"))
    val dilated = foregroundCount(byName("d2-dilate"))
    val boxDilated = foregroundCount(byName("d2-box-dilate"))
    val diskDilated = foregroundCount(byName("d2-disk-dilate"))

    assert(eroded < threshold, "erosion must remove foreground pixels")
    assert(dilated > threshold, "dilation must add foreground pixels")
    assert(boxDilated > threshold, "box dilation must add foreground pixels")
    assert(diskDilated > boxDilated, "the larger disk must have a larger footprint")
    assertNotEquals(
      byName("d2-box-dilate"),
      byName("d2-disk-dilate"),
      "box and disk neighborhoods must remain visually distinguishable"
    )

  test("Gaussian impulse rendering is centered, symmetric, and visibly spread"):
    val raster = byName("d2-gaussian-impulse")
    def gray(x: Int, y: Int): Int = raster.pixelUnsafe(x, y).red

    assert(gray(4, 4) > gray(3, 4), "the impulse center must remain brightest")
    assert(gray(3, 4) > gray(2, 4), "the blur must decay away from the center")
    assertEquals(gray(3, 4), gray(5, 4))
    assertEquals(gray(4, 3), gray(4, 5))
    assertEquals(gray(3, 3), gray(5, 5))
    assert(gray(2, 2) > 0, "the rendered blur must reach beyond immediate neighbors")

  test("Sobel and Scharr renderings contain directional visual structure"):
    Vector(
      "d2-sobel-x",
      "d2-sobel-y",
      "d2-scharr-x",
      "d2-scharr-y"
    ).foreach { name =>
      val values = grayscaleValues(byName(name))
      assert(
        values.max - values.min > 20,
        s"$name raster should contain visible directional contrast"
      )
    }

  test("asymmetric correlation and convolution render opposite orientations"):
    val correlation = byName("d2-correlation-asymmetric")
    val convolution = byName("d2-convolution-asymmetric")

    assertNotEquals(correlation, convolution)
    assert(
      peakX(correlation) < 4,
      "correlation should place the largest asymmetric weight to the left"
    )
    assert(
      peakX(convolution) > 4,
      "convolution should place the largest asymmetric weight to the right"
    )

  test("Gaussian extent renderings expose Valid and Full output geometry"):
    val same = byName("d2-gaussian-impulse")
    val valid = byName("d2-gaussian-valid")
    val full = byName("d2-gaussian-full")

    assert(valid.width < same.width)
    assert(valid.height < same.height)
    assert(full.width > same.width)
    assert(full.height > same.height)

  test("D3 orthogonal slices preserve source axes and operation structure"):
    assertEquals((byName("d3-source-x").width, byName("d3-source-x").height), (6, 5))
    assertEquals((byName("d3-source-y").width, byName("d3-source-y").height), (7, 5))
    assertEquals((byName("d3-source-z").width, byName("d3-source-z").height), (7, 6))

    val gradientValues = grayscaleValues(byName("d3-sobel-z"))
    assert(
      gradientValues.max - gradientValues.min > 20,
      "D3 Sobel slice should contain visible directional contrast"
    )

    val threshold = byName("d3-threshold-z")
    val dilated = byName("d3-ball-dilate-z")
    val closed = byName("d3-ball-close-z")
    assert(gray(threshold, 3, 3) < 128, "the D3 threshold slice must retain its hole")
    assert(gray(threshold, 2, 2) > 128, "the D3 threshold slice must retain foreground")
    assert(gray(dilated, 3, 3) > 128, "ball dilation must fill the D3 hole")
    assert(gray(closed, 3, 3) > 128, "ball closing must fill the D3 hole")

  private def maskAscii(raster: RasterImage): Vector[String] =
    Vector.tabulate(raster.height) { y =>
      Vector
        .tabulate(raster.width) { x =>
          if raster.pixelUnsafe(x, y).alpha == 0 then '.' else '#'
        }
        .mkString
    }

  private def foregroundCount(raster: RasterImage): Int =
    Vector
      .tabulate(raster.width * raster.height) { index =>
        if raster.pixelUnsafe(index % raster.width, index / raster.width).alpha == 0
        then 0
        else 1
      }
      .sum

  private def grayscaleValues(raster: RasterImage): Vector[Int] =
    Vector.tabulate(raster.width * raster.height) { index =>
      raster.pixelUnsafe(index % raster.width, index / raster.width).red
    }

  private def peakX(raster: RasterImage): Int =
    var bestX = 0
    var bestValue = Int.MinValue
    var y = 0
    while y < raster.height do
      var x = 0
      while x < raster.width do
        val value = raster.pixelUnsafe(x, y).red
        if value > bestValue then
          bestValue = value
          bestX = x
        x += 1
      y += 1
    bestX

  private def gray(raster: RasterImage, x: Int, y: Int): Int =
    raster.pixelUnsafe(x, y).red
