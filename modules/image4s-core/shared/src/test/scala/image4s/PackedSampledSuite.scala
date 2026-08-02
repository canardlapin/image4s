package image4s

import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import ravel.packed.PackedBits

final class PackedSampledSuite extends FunSuite:
  test("packed and unpacked mask code sequences agree"):
    val values = Vector.tabulate(35)(index => (index * 5) % 7 < 3)
    val mask = maskImage(Vector(5, 7), values)
    val packed = packedRight(PackedSampled.packMask(mask))

    assertEquals(packed.logicalShape, Vector(5, 7))
    assertEquals(packed.codeVector, values.map(if _ then 1 else 0))
    assertEquals(packed.decodeVector, values)
    assertEquals(
      packed.decodeToFloatArray.toVector,
      values.map(if _ then 1.0f else 0.0f)
    )

  test("wordwise mask algebra matches dense Boolean semantics"):
    val leftValues = Vector.tabulate(40)(index => (index * 5) % 7 < 3)
    val rightValues = Vector.tabulate(40)(index => (index * 3) % 5 < 2)
    val space = maskImage(Vector(5, 8), leftValues)
    val leftPacked = packedRight(PackedSampled.packMask(space))
    val rightPacked =
      packedRight(
        PackedSampled.packMask(
          imageRight(
            Sampled.mask(
              space.sampleSpace,
              NDArray.fromSeq(Shape(5, 8), rightValues)
            )
          )
        )
      )

    assertEquals(
      packedRight(leftPacked.union(rightPacked)).decodeVector,
      leftValues.zip(rightValues).map(_ || _)
    )
    assertEquals(
      packedRight(leftPacked.intersection(rightPacked)).decodeVector,
      leftValues.zip(rightValues).map(_ && _)
    )
    assertEquals(
      packedRight(leftPacked.difference(rightPacked)).decodeVector,
      leftValues.zip(rightValues).map((l, r) => l && !r)
    )
    assertEquals(
      packedRight(leftPacked.symmetricDifference(rightPacked)).decodeVector,
      leftValues.zip(rightValues).map(_ != _)
    )
    assertEquals(
      packedRight(leftPacked.complement).decodeVector,
      leftValues.map(!_)
    )
    assertEquals(
      packedRight(leftPacked.countTrue),
      leftValues.count(identity).toLong
    )

  test("mask algebra rejects operands from a different grid"):
    val values = Vector.fill(35)(true)
    val first = packedRight(PackedSampled.packMask(maskImage(Vector(5, 7), values)))
    val second = packedRight(PackedSampled.packMask(maskImage(Vector(5, 7), values)))

    first.union(second) match
      case Left(PackedImageError.SpaceMismatch(_, _)) => ()
      case other => fail(s"expected SpaceMismatch, got $other")

  test("label codes round-trip and overflowing labels are rejected"):
    val labels = Vector.tabulate(30)(index => index % 16)
    val image = labelImage(Vector(5, 6), labels)
    val packed =
      packedRight(PackedSampled.pack(image, PackedEncoding.Labels(PackedBits.B4)))

    assertEquals(packed.codeVector, labels)
    assertEquals(packed.decodeVector, labels)

    val overflowing = labelImage(Vector(5, 6), labels.updated(7, 16))
    PackedSampled.pack(overflowing, PackedEncoding.Labels(PackedBits.B4)) match
      case Left(PackedImageError.Packed(_)) => ()
      case other => fail(s"expected packed code overflow, got $other")

  test("uniform quantizer honours the half-step error bound in window"):
    val quantizer =
      packedRight(
        PackedEncoding.UniformQuantizer.create(-1.0, 1.0, PackedBits.B4)
      )
    val values =
      Vector.tabulate(30)(index => -1.0 + 2.0 * index.toDouble / 29.0)
    val image = scalarImage(Vector(5, 6), values)
    val packed = packedRight(PackedSampled.pack(image, quantizer))

    packed.decodeVector.zip(values).foreach { (reconstructed, original) =>
      assert(
        math.abs(reconstructed - original) <= quantizer.step / 2.0 + 1.0e-12,
        s"|$reconstructed - $original| exceeds half step ${quantizer.step / 2.0}"
      )
    }

  test("uniform quantizer saturates outside its window"):
    val quantizer =
      packedRight(
        PackedEncoding.UniformQuantizer.create(0.0, 3.0, PackedBits.B2)
      )

    assertEquals(quantizer.encode(-100.0), 0)
    assertEquals(quantizer.encode(100.0), 3)
    assertEquals(quantizer.decode(quantizer.encode(-100.0)), 0.0)
    assertEquals(quantizer.decode(quantizer.encode(100.0)), 3.0)
    assert(
      PackedEncoding.UniformQuantizer
        .create(2.0, 2.0, PackedBits.B2)
        .isLeft
    )

  test("packed code views address bits like the code subsequence"):
    val labels = Vector.tabulate(24)(index => index % 16)
    val image = labelImage(Vector(4, 6), labels)
    val packed =
      packedRight(PackedSampled.pack(image, PackedEncoding.Labels(PackedBits.B4)))
    val narrowed = packed.codes.narrow(0, 1, 2).flatMap(_.narrow(1, 2, 3))

    narrowed match
      case Right(view) =>
        assertEquals(
          view.codeVector,
          Vector.tabulate(6)(linear => labels((1 + linear / 3) * 6 + 2 + linear % 3))
        )
      case Left(error) => fail(error.message)

  private def maskImage(shape: Vector[Int], values: Vector[Boolean]) =
    val space = sampleSpace(shape)
    imageRight(
      Sampled.mask(space, NDArray.fromSeq(Shape(shape(0), shape(1)), values))
    )

  private def labelImage(shape: Vector[Int], values: Vector[Int]) =
    val space = sampleSpace(shape)
    imageRight(
      Sampled.categorical[Int, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def scalarImage(shape: Vector[Int], values: Vector[Double]) =
    val space = sampleSpace(shape)
    imageRight(
      Sampled.continuous[Double, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def sampleSpace(shape: Vector[Int]) =
    val frame = geometryRight(Frame.named[D2]("packed-sampled"))
    val grid = geometryRight(
      image4s.geometry.Grid.in(frame)(shape, Affine.identity[D2])
    )
    SampleSpace.create(grid, NonSpatialAxes.empty)

  private def packedRight[A](value: Either[PackedImageError, A]): A =
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
