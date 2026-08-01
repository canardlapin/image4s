package image4s

import image4s.geometry.Affine
import image4s.geometry.CoordinateConvention
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LengthUnit
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Shape

final class ApproachableApiSuite extends FunSuite:
  test("ordinary construction, time selection, crop, and map stay concise"):
    val frame =
      geometryRight(
        Frame.named[D3](
          "native",
          unit = LengthUnit.Millimeter,
          convention = CoordinateConvention.RAS
        )
      )
    val grid =
      geometryRight(
        Grid.in(frame)(
          Vector(6, 7, 5),
          Affine.identity[D3]
        )
      )
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          extent = 4,
          origin = 0.0,
          step = 0.8,
          unit = AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val values =
      NDArray.tabulate[Double](6, 7, 5, 4)((i, j, k, t) =>
        1000.0 * i + 100.0 * j + 10.0 * k + t
      )
    val bold = imageRight(Image.continuous(grid, axes, values))
    val volume = imageRight(bold.atTime(2))
    val crop =
      imageRight(
        volume.crop(
          origin = Vector(1, 2, 1),
          shape = Vector(3, 4, 2)
        )
      )
    val centered = crop.mapValues(_ - 1000.0)

    assertEquals(bold.logicalShape, Vector(6, 7, 5, 4))
    assertEquals(volume.logicalShape, Vector(6, 7, 5))
    assertEquals(crop.logicalShape, Vector(3, 4, 2))
    assertEquals(crop(0, 0, 0), bold(1, 2, 1, 2))
    assertEquals(centered(0, 0, 0), crop(0, 0, 0) - 1000.0)
    assert(!crop.data.isCanonicalLayout)

  test("same-owner and aligned zip expose proof bookkeeping only when needed"):
    val frame =
      geometryRight(Frame.named[D3]("pointwise"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2, 2), Affine.identity[D3])
      )
    val leftSpace = SampleSpace.create(grid, NonSpatialAxes.empty)
    val rightSpace = SampleSpace.create(grid, NonSpatialAxes.empty)
    val left =
      imageRight(
        Image.continuous(
          leftSpace,
          NDArray.fromSeq(
            Shape(2, 2, 2),
            Vector.tabulate(8)(_.toDouble)
          )
        )
      )
    val sameOwner =
      imageRight(
        Image.continuous(
          leftSpace,
          NDArray.fromSeq(
            Shape(2, 2, 2),
            Vector.fill(8)(10.0)
          )
        )
      )
    val reconstructed =
      imageRight(
        Image.continuous(
          rightSpace,
          NDArray.fromSeq(
            Shape(2, 2, 2),
            Vector.fill(8)(20.0)
          )
        )
      )

    val direct = left.zipWith(sameOwner)(_ + _)
    val alignment = imageRight(leftSpace.alignExact(rightSpace))
    val rebound = reconstructed.rebind(alignment.reverse)
    val aligned = left.zipWith(rebound)(_ + _)
    val checked = left.zipWithAligned(reconstructed, alignment)(_ + _)

    assertEquals(direct(1, 1, 1), 17.0)
    assertEquals(aligned(1, 1, 1), 27.0)
    assertEquals(checked(1, 1, 1), 27.0)
    assert(rebound.data eq reconstructed.data)
    assert(checked.sampleSpace eq leftSpace)

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
