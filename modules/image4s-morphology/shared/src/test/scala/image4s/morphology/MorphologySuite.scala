package image4s.morphology

import image4s.Axis
import image4s.AxisKind
import image4s.ImageError
import image4s.Mask
import image4s.NonSpatialAxes
import image4s.SampleSpace
import image4s.Sampled
import image4s.ValueSemantics
import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.ops.OpError
import image4s.ops.Radius
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray
import ravel.Rank
import ravel.Shape
import ravel.map

final class MorphologySuite extends FunSuite:
  test("threshold converts continuous samples to a Boolean Mask"):
    val image =
      continuous2D(
        Vector(0.0f, 1.0f, 2.0f, 3.0f),
        Vector(2, 2)
      )
    val result =
      opsRight(image.threshold(2.0f, ThresholdComparison.GreaterOrEqual))

    assertEquals(
      result.data.elementsIterator.toVector,
      Vector(false, false, true, true)
    )
    assertEquals(result.logicalShape, image.logicalShape)
    assert(result.grid.sameRuntimeOwnerAs(image.grid))

  test("threshold keeps custom Ordering semantics off the primitive fast path"):
    val image =
      continuous2D(
        Vector(1.0f, 2.0f, 3.0f, 4.0f),
        Vector(2, 2)
      )
    val descending = Ordering.Float.IeeeOrdering.reverse
    val result =
      opsRight(
        image.threshold(
          2.0f,
          ThresholdComparison.GreaterOrEqual
        )(using descending, summon[ValueSemantics[Boolean, Mask]])
      )

    assertEquals(
      result.data.elementsIterator.toVector,
      Vector(true, true, false, false)
    )

  test("sample-radius box erosion and dilation use primitive Boolean neighborhoods"):
    val source =
      mask2D(
        Vector.tabulate(25)(index => index == 12),
        Vector(5, 5)
      )
    val element = StructuringElement.box[D2](opsRight(Radius.samples(1)))
    val dilated = opsRight(source.dilate(element))

    val expectedDilated =
      Vector.tabulate(25) { index =>
        val row = index / 5
        val column = index % 5
        row >= 1 && row <= 3 && column >= 1 && column <= 3
      }
    assertEquals(dilated.data.elementsIterator.toVector, expectedDilated)

    val eroded = opsRight(dilated.erode(element))
    assertEquals(eroded.data.elementsIterator.toVector, Vector.tabulate(25)(_ == 12))

  test("opening and closing satisfy extent and idempotence laws"):
    val source =
      mask2D(
        Vector(
          false, false, false, false, false,
          false, true, true, false, false,
          false, true, false, true, false,
          false, false, true, true, false,
          false, false, false, false, false
        ),
        Vector(5, 5)
      )
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val opened = opsRight(source.open(element))
    val openedAgain = opsRight(opened.open(element))
    val closed = opsRight(source.close(element))
    val closedAgain = opsRight(closed.close(element))

    assertSubset(opened, source, "opening must be anti-extensive")
    assertSubset(source, closed, "closing must be extensive")
    assertEquals(
      openedAgain.data.elementsIterator.toVector,
      opened.data.elementsIterator.toVector
    )
    assertEquals(
      closedAgain.data.elementsIterator.toVector,
      closed.data.elementsIterator.toVector
    )

  test("dilation and erosion obey complement duality with default boundaries"):
    val source =
      mask2D(
        Vector(
          false, true, false,
          true, false, false,
          false, false, true
        ),
        Vector(3, 3)
      )
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val complementSource = complement(source)
    val dualDilate = complement(opsRight(source.dilate(element)))
    val erodeComplement = opsRight(complementSource.erode(element))

    assertEquals(
      dualDilate.data.elementsIterator.toVector,
      erodeComplement.data.elementsIterator.toVector
    )

  test("frame-radius disk uses the affine-induced metric"):
    val frame = geometryRight(Frame.named[D2]("physical-disk"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D2](
          origin = Vector(0.0, 0.0),
          spacing = Vector(2.0, 1.0),
          directionRowMajor = Vector(1.0, 0.0, 0.0, 1.0)
        )
      )
    val grid = geometryRight(Grid.in(frame)(Vector(7, 7), affine))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val source =
      imageRight(
        Sampled.mask(
          space,
          NDArray.tabulate[Boolean](7, 7)((row, column) =>
            row == 3 && column == 3
          )
        )
      )
    val element = StructuringElement.disk[D2](opsRight(Radius.frame(2.0)))
    val dilated = opsRight(source.dilate(element))

    assert(dilated.data(2, 3), "one row step is two frame units")
    assert(dilated.data(3, 1), "two column steps are two frame units")
    assert(!dilated.data(2, 2), "diagonal physical distance exceeds radius")
    assert(!dilated.data(1, 3), "two row steps exceed radius")

  test("binary morphology processes non-spatial axes independently"):
    val frame = geometryRight(Frame.named[D2]("morphology-batch"))
    val grid = geometryRight(Grid.in(frame)(Vector(3, 3), Affine.identity[D2]))
    val channels =
      imageRight(Axis.ordinal("channel", AxisKind.Channel, 2))
    val axes = imageRight(NonSpatialAxes.from(Vector(channels)))
    val space = SampleSpace.create(grid, axes)
    val source =
      imageRight(
        Sampled.mask(
          space,
          NDArray.tabulate[Boolean](3, 3, 2) { (row, column, channel) =>
            channel == 0 && row == 1 && column == 1
          }
        )
      )
    val element = StructuringElement.cross[D2](opsRight(Radius.samples(1)))
    val dilated = opsRight(source.dilate(element))

    assertEquals(dilated.data.elementsIterator.count(identity), 5)
    var row = 0
    while row < 3 do
      var column = 0
      while column < 3 do
        assert(!dilated.data(row, column, 1), "channel one must remain empty")
        column += 1
      row += 1

  test("D3 ball uses the expected discrete support"):
    val frame = geometryRight(Frame.named[D3]("ball-batch"))
    val grid = geometryRight(Grid.in(frame)(Vector(3, 3, 3), Affine.identity[D3]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    val data =
      NDArray.tabulate[Boolean](3, 3, 3)((x, y, z) => x == 1 && y == 1 && z == 1)
    val source = imageRight(Sampled.mask(space, data))
    val element = StructuringElement.ball[D3](opsRight(Radius.samples(1)))
    val dilated = opsRight(source.dilate(element))

    assertEquals(dilated.data.elementsIterator.count(identity), 7)
    assert(dilated.data(0, 1, 1))
    assert(!dilated.data(0, 0, 1))

  test("prepared opening reuses support and does not alias earlier output"):
    val source =
      mask2D(
        Vector.tabulate(25)(index => index == 12 || index == 13),
        Vector(5, 5)
      )
    val next =
      imageRight(
        Sampled.mask(
          source.sampleSpace,
          NDArray.fromSeq(Shape(5, 5), Vector.fill(25)(true))
        )
      )
    val element = StructuringElement.box[D2](opsRight(Radius.samples(1)))
    val plan = opsRight(BinaryMorphology.prepareOpen(source, element))
    val first = opsRight(plan.run(source))
    val firstValues = first.data.elementsIterator.toVector
    val second = opsRight(plan.run(next))

    assertEquals(first.data.elementsIterator.toVector, firstValues)
    assert(plan.support.nonEmpty)
    assert(second.data.elementsIterator.forall(identity))

  private def continuous2D(
      values: Vector[Float],
      shape: Vector[Int]
  ) =
    val frame = geometryRight(Frame.named[D2]("continuous-mask"))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.continuous[Float, Rank[2]](
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def mask2D(values: Vector[Boolean], shape: Vector[Int]) =
    val frame = geometryRight(Frame.named[D2]("binary-mask"))
    val grid = geometryRight(Grid.in(frame)(shape, Affine.identity[D2]))
    val space = SampleSpace.create(grid, NonSpatialAxes.empty)
    imageRight(
      Sampled.mask(
        space,
        NDArray.fromSeq(Shape(shape(0), shape(1)), values)
      )
    )

  private def complement[R <: ravel.AnyRank](
      image: image4s.MaskImage[? <: SampleSpace[?, ?], R]
  ) =
    imageRight(
      Sampled.mask(
        image.sampleSpace,
        image.data.map(value => !value),
        image.metadata
      )
    )

  private def assertSubset[
      S1 <: SampleSpace[?, ?],
      S2 <: SampleSpace[?, ?],
      R <: ravel.AnyRank
  ](
      subset: image4s.MaskImage[S1, R],
      superset: image4s.MaskImage[S2, R],
      clue: String
  ): Unit =
    subset.data.elementsIterator
      .zip(superset.data.elementsIterator)
      .foreach { case (left, right) =>
        assert(!left || right, clue)
      }

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
