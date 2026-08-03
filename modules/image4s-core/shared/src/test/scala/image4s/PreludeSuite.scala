package image4s.tests

import image4s.prelude.*
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

final class PreludeSuite extends FunSuite:
  test("prelude supports continuous, categorical, and mask construction"):
    val sampling =
      SamplingSpec[D2](
        frame = FrameSpec.named(
          "prelude",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.LPS
        ),
        grid = GridSpec.identity,
        axes = AxesSpec.empty
      )
    val space = imageRight(sampling.buildFor(Vector(2, 3)))
    val continuous =
      imageRight(
        Image.continuous(
          space,
          ravel.NDArray.tabulate[Double](2, 3)((row, column) => row + column)
        )
      )
    val categorical =
      imageRight(
        Image.categorical(
          space,
          ravel.NDArray.tabulate[Int](2, 3)((row, column) => row + column)
        )
      )
    val mask =
      imageRight(
        Image.mask(
          space,
          ravel.NDArray.tabulate[Boolean](2, 3)((row, column) => row == column)
        )
      )

    assertEquals(continuous.logicalShape, Vector(2, 3))
    assertEquals(categorical.logicalShape, Vector(2, 3))
    assertEquals(mask.logicalShape, Vector(2, 3))
    assertEquals(continuous(1, 2), 3.0)
    assertEquals(categorical(1, 2), 3)
    assert(!mask(1, 2))

  test("mixed direct and prelude imports retain one dtype instance"):
    assert(
      typeCheckErrors(
        """
          import image4s.prelude.*
          import image4s.geometry.D2
          import ravel.DType.given
          import ravel.NDArray
          val values = NDArray.tabulate[Double](2, 2)((row, column) => row + column)
          val request = SamplingSpec[D2](FrameSpec.named("mixed"), GridSpec.identity)
          val result = Image.continuous(values, request)
        """
      ).isEmpty
    )

  test("prelude does not export data containers or feature modules"):
    assert(typeCheckErrors("import image4s.prelude.*; val value: NDArray[?, ?] = ???").nonEmpty)
    assert(typeCheckErrors("import image4s.prelude.*; val value = SpatialSigma").nonEmpty)
    assert(typeCheckErrors("import image4s.prelude.*; val value = Nifti").nonEmpty)

  private def imageRight[A](value: Either[ImageError, A]): A =
    value match
      case Right(result) => result
      case Left(error) => fail(error.message)
