package image4s.ops.laws

import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.filter.Gradient
import image4s.filter.gaussianBlur
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.morphology.StructuringElement
import image4s.morphology.close
import image4s.morphology.open
import image4s.morphology.threshold
import image4s.ops.FrameCoordinates
import image4s.ops.IndexCoordinates
import image4s.ops.OpError
import image4s.ops.Radius
import image4s.ops.SpatialSigma
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank

/** The epic's demonstration pipeline: frame-scaled Gaussian smoothing, frame
  * gradients, thresholding to a mask, and binary opening/closing, on D2 and D3
  * anisotropic grids.
  */
final class OpsVerticalSliceSuite extends FunSuite:
  test("D2 vertical slice: Gaussian(frame) -> frame gradient -> threshold -> open/close"):
    val spacing = Vector(2.0, 1.0)
    val frame = geometryRight(Frame.named[D2]("vertical-slice-d2"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(0.0, 0.0),
          spacing = spacing,
          directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
        )
      )
    val grid = geometryRight(Grid.in(frame)(Vector(12, 12), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[2]](
          space,
          NDArray.tabulate[Double](12, 12) { (row, column) =>
            if row == 5 && column == 5 then 0.0
            else if row >= 3 && row <= 8 && column >= 3 && column <= 8 then 10.0
            else if row == 1 && column == 10 then 40.0
            else 0.0
          }
        )
      )

    val sigma = opsRight(SpatialSigma.frame[D2](0.5))
    val blurred = opsRight(image.gaussianBlur(sigma))
    assert(blurred.grid.sameRuntimeOwnerAs(image.grid))

    val indexField = opsRight(Gradient.sobel(blurred, IndexCoordinates))
    val frameField = opsRight(Gradient.sobel(blurred, FrameCoordinates))
    var axis = 0
    while axis < 2 do
      indexField
        .component(axis)
        .get
        .data
        .elementsIterator
        .zip(frameField.component(axis).get.data.elementsIterator)
        .foreach { case (index, frameValue) =>
          assertEqualsDouble(
            frameValue,
            index / spacing(axis),
            1.0e-9,
            s"frame gradient must divide by spacing on axis $axis"
          )
        }
      axis += 1

    val mask = opsRight(blurred.threshold(5.0))
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val opened = opsRight(mask.open(element))
    val closed = opsRight(mask.close(element))

    assert(mask.data(1, 10), "the bright speck must survive thresholding")
    assert(!mask.data(5, 5), "the hole must survive thresholding")
    assert(!opened.data(1, 10), "opening must remove the isolated speck")
    assert(opened.data(4, 4), "opening must preserve the plateau interior")
    assert(closed.data(5, 5), "closing must fill the interior hole")
    opened.data.elementsIterator
      .zip(mask.data.elementsIterator)
      .foreach { case (o, m) => assert(!o || m, "opening must be anti-extensive") }
    mask.data.elementsIterator
      .zip(closed.data.elementsIterator)
      .foreach { case (m, c) => assert(!m || c, "closing must be extensive") }

  test("D3 vertical slice: Gaussian(frame) -> frame gradient -> threshold -> open/close"):
    val spacing = Vector(1.0, 2.0, 1.0)
    val frame = geometryRight(Frame.named[D3]("vertical-slice-d3"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D3](
          origin = Vector(0.0, 0.0, 0.0),
          spacing = spacing,
          directionRowMajor =
            Vector(1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)
        )
      )
    val grid = geometryRight(Grid.in(frame)(Vector(9, 9, 9), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val image =
      imageRight(
        Sampled.continuous[Double, Rank[3]](
          space,
          NDArray.tabulate[Double](9, 9, 9) { (x, y, z) =>
            if x == 4 && y == 4 && z == 4 then 0.0
            else if x >= 2 && x <= 6 && y >= 2 && y <= 6 && z >= 2 && z <= 6
            then 10.0
            else if x == 1 && y == 1 && z == 7 then 40.0
            else 0.0
          }
        )
      )

    val sigma = opsRight(SpatialSigma.frame[D3](0.5))
    val blurred = opsRight(image.gaussianBlur(sigma))
    assert(blurred.grid.sameRuntimeOwnerAs(image.grid))

    val indexField = opsRight(Gradient.sobel(blurred, IndexCoordinates))
    val frameField = opsRight(Gradient.sobel(blurred, FrameCoordinates))
    var axis = 0
    while axis < 3 do
      indexField
        .component(axis)
        .get
        .data
        .elementsIterator
        .zip(frameField.component(axis).get.data.elementsIterator)
        .foreach { case (index, frameValue) =>
          assertEqualsDouble(
            frameValue,
            index / spacing(axis),
            1.0e-9,
            s"frame gradient must divide by spacing on axis $axis"
          )
        }
      axis += 1

    val mask = opsRight(blurred.threshold(5.0))
    val element = StructuringElement.cross[D3](opsRight(Radius.samples(1)))
    val opened = opsRight(mask.open(element))
    val closed = opsRight(mask.close(element))

    assert(mask.data(1, 1, 7), "the bright speck must survive thresholding")
    assert(!mask.data(4, 4, 4), "the hole must survive thresholding")
    assert(!opened.data(1, 1, 7), "opening must remove the isolated speck")
    assert(opened.data(3, 3, 3), "opening must preserve the plateau interior")
    assert(closed.data(4, 4, 4), "closing must fill the interior hole")
    opened.data.elementsIterator
      .zip(mask.data.elementsIterator)
      .foreach { case (o, m) => assert(!o || m, "opening must be anti-extensive") }
    mask.data.elementsIterator
      .zip(closed.data.elementsIterator)
      .foreach { case (m, c) => assert(!m || c, "closing must be extensive") }

  private def opsRight[A](value: Either[OpError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def geometryRight[A](value: Either[GeometryError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error)   => fail(error.message)
