package image4s

import munit.FunSuite
import ravel.Shape
import ravel.js.JsInterop
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import scala.scalajs.js.typedarray.Float64Array

final class BorrowedJsSuite extends FunSuite:
  test("a borrowed JavaScript typed array is copied before Sampled owns it"):
    val frame = geometryRight(Frame.named[D2]("borrowed-js"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val callerOwned = new Float64Array(4)
    callerOwned(0) = 1.0
    callerOwned(1) = 2.0
    callerOwned(2) = 3.0
    callerOwned(3) = 4.0
    val borrowed =
      JsInterop.unsafeBorrow(callerOwned, Shape(2, 2))
    val sampled =
      imageRight(
        Sampled.copyContinuousFromBorrowed(
          grid,
          NonSpatialAxes.empty,
          borrowed
        )
      )
    callerOwned(0) = 99.0

    assertEquals(imageRight(sampled.valueAt(Vector(0, 0))), 1.0)

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
