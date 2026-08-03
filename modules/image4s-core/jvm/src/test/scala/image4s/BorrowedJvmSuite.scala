package image4s

import munit.FunSuite
import ravel.DType.given
import ravel.Shape
import ravel.jvm.JvmInterop
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

final class BorrowedJvmSuite extends FunSuite:
  test("a borrowed JVM array is copied before Sampled owns it"):
    val frame = geometryRight(Frame.named[D2]("borrowed-jvm"))
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 2), Affine.identity[D2]))
    val callerOwned = Array(1.0, 2.0, 3.0, 4.0)
    val borrowed =
      JvmInterop.unsafeBorrow(callerOwned, Shape(2, 2))
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
      case Left(error) => fail(error.message)

  private def imageRight[A](
      value: Either[ImageError, A]
  ): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
