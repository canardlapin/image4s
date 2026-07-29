package image4s.laws

import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.Sampled
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid

final class ImageLawsSuite extends ScalaCheckSuite:
  private val frame =
    geometryRight(Frame.named[D2]("image-law-frame"))

  property("every constructed Sampled value obeys its shape law"):
    forAll(
      Gen.choose(1, 8),
      Gen.choose(1, 8)
    ): (firstExtent, secondExtent) =>
      val grid =
        geometryRight(
          Grid.in(frame)(
            Vector(firstExtent, secondExtent),
            Affine.identity[D2]
          )
        )
      val data =
        NDArray.zeros[Double, Rank[2]](
          Shape(firstExtent, secondExtent)
        )
      val sampled =
        imageRight(Sampled.scalar(grid, NonSpatialAxes.empty, data))

      assert(ImageLaws.shapeAgrees(sampled))
      assert(sampled.data eq data)

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
