package image4s

import image4s.geometry.Affine
import image4s.geometry.D2
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.LatticeIndex
import munit.FunSuite
import ravel.DType.given
import ravel.NDArray

final class LatticeMapSuite extends FunSuite:
  test("strided permutation preserves every mapped physical point and value"):
    val frame = geometryRight(Frame.named[D2]("exact-map"))
    val affine =
      geometryRight(
        Affine.fromRowMajor[D2](
          Vector(
            2.0, 0.25, 10.0, -0.5, 3.0, 20.0, 0.0, 0.0, 1.0
          )
        )
      )
    val grid =
      geometryRight(Grid.in(frame)(Vector(5, 6), affine))
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time)))
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](5, 6, 2)((i, j, t) => 100.0 * i + 10.0 * j + t)
        )
      )
    val map =
      imageRight(
        LatticeMap.stridedPermutation[D2](
          sourceShape = Vector(5, 6),
          targetShape = Vector(3, 3),
          origin = Vector(4, 1),
          sourceAxisForTarget = Vector(1, 0),
          steps = Vector(2, -2)
        )
      )
    val target = imageRight(source.view(map))

    assertEquals(target.grid.shape, Vector(3, 3))
    assertEquals(target.logicalShape, Vector(3, 3, 2))
    assert(!target.data.isCanonicalLayout)
    for
      first <- 0 until 3
      second <- 0 until 3
    do
      val targetCoordinate = Vector(first, second)
      val sourceCoordinate =
        imageRight(map.sourceIndex(targetCoordinate))
      val targetPoint =
        geometryRight(
          target.grid.pointAt(
            geometryRight(LatticeIndex.fromVector[D2](targetCoordinate))
          )
        )
      val sourcePoint =
        geometryRight(
          source.grid.pointAt(
            geometryRight(LatticeIndex.fromVector[D2](sourceCoordinate))
          )
        )
      assertEquals(targetPoint.coordinates, sourcePoint.coordinates)
      for timeIndex <- 0 until 2 do
        assertEquals(
          imageRight(
            target.valueAt(targetCoordinate, Vector(timeIndex))
          ),
          imageRight(
            source.valueAt(sourceCoordinate, Vector(timeIndex))
          )
        )

  test("identity, composition, flip, permutation, and crop obey pullback laws"):
    val identity =
      imageRight(LatticeMap.identity[D2](Vector(6, 7)))
    val flip =
      imageRight(LatticeMap.flip[D2](Vector(6, 7), axis = 0))
    val doubleFlip = imageRight(flip.followedBy(flip))
    val permutation =
      imageRight(LatticeMap.permute[D2](Vector(6, 7), Vector(1, 0)))
    val inversePermutation =
      imageRight(LatticeMap.permute[D2](Vector(7, 6), Vector(1, 0)))
    val permutedBack =
      imageRight(permutation.followedBy(inversePermutation))
    val outer =
      imageRight(
        LatticeMap.crop[D2](
          Vector(6, 7),
          Vector(1, 1),
          Vector(4, 5)
        )
      )
    val inner =
      imageRight(
        LatticeMap.crop[D2](
          Vector(4, 5),
          Vector(1, 2),
          Vector(2, 2)
        )
      )
    val composed = imageRight(outer.followedBy(inner))

    assert(identity.isIdentity)
    assert(doubleFlip.isIdentity)
    assert(permutedBack.isIdentity)
    for
      first <- 0 until 2
      second <- 0 until 2
    do
      val target = Vector(first, second)
      val sequential =
        imageRight(
          outer.sourceIndex(imageRight(inner.sourceIndex(target)))
        )
      assertEquals(
        imageRight(composed.sourceIndex(target)),
        sequential
      )
      assertEquals(sequential, Vector(first + 2, second + 3))

    val frame = geometryRight(Frame.named[D2]("view-laws"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(6, 7), Affine.identity[D2])
      )
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](6, 7)((i, j) => 10.0 * i + j)
        )
      )
    val identityView = imageRight(source.view(identity))
    val sequentialView =
      imageRight(imageRight(source.view(outer)).view(inner))
    val composedView = imageRight(source.view(composed))
    val flippedTwice =
      imageRight(
        imageRight(source.flipSpatial(0)).flipSpatial(0)
      )
    val permutedTwice =
      imageRight(
        imageRight(source.permuteSpatial(Vector(1, 0)))
          .permuteSpatial(Vector(1, 0))
      )

    assert(identityView.sampleSpace eq source.sampleSpace)
    assert(identityView.data eq source.data)
    assert(
      sequentialView.sameValuesAs(composedView)(_ == _)
    )
    assert(flippedTwice.sameValuesAs(source)(_ == _))
    assert(permutedTwice.sameValuesAs(source)(_ == _))

  test("spatial stride is an exact zero-copy view facade"):
    val frame = geometryRight(Frame.named[D2]("stride"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(7, 8), Affine.identity[D2])
      )
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](7, 8)((i, j) => 10.0 * i + j)
        )
      )
    val target = imageRight(source.strideSpatial(Vector(3, 2)))

    assertEquals(target.grid.shape, Vector(3, 4))
    assert(!target.data.isCanonicalLayout)
    for
      first <- 0 until 3
      second <- 0 until 4
    do
      assertEquals(
        imageRight(target.valueAt(Vector(first, second))),
        imageRight(source.valueAt(Vector(first * 3, second * 2)))
      )

  test("non-spatial permutation moves coordinates and values together"):
    val frame = geometryRight(Frame.named[D2]("axis-permutation"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3), Affine.identity[D2])
      )
    val time =
      imageRight(
        Axis.regular(
          "time",
          AxisKind.Time,
          2,
          0.0,
          0.8,
          AxisUnit.Seconds
        )
      )
    val channel =
      imageRight(
        Axis.categorical(
          "channel",
          AxisKind.Channel,
          Vector("red", "green", "blue")
        )
      )
    val axes = imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](2, 3, 2, 3)((i, j, t, c) =>
            1000.0 * i + 100.0 * j + 10.0 * t + c
          )
        )
      )
    val identity = imageRight(source.permuteNonSpatial(Vector(0, 1)))
    val target = imageRight(source.permuteNonSpatial(Vector(1, 0)))
    val roundTrip =
      imageRight(target.permuteNonSpatial(Vector(1, 0)))

    assert(identity.sampleSpace eq source.sampleSpace)
    assert(identity.data eq source.data)
    assertEquals(
      target.nonSpatialAxes.records,
      Vector(channel.record, time.record)
    )
    assertEquals(target.logicalShape, Vector(2, 3, 3, 2))
    assertEquals(
      imageRight(target.valueAt(Vector(1, 2), Vector(2, 1))),
      imageRight(source.valueAt(Vector(1, 2), Vector(1, 2)))
    )
    assert(roundTrip.sameValuesAs(source)(_ == _))
    assertEquals(roundTrip.nonSpatialAxes.records, axes.records)

  test("mapValues, checked replacement, and reduction preserve sampling laws"):
    val frame = geometryRight(Frame.named[D2]("field-algebra"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 3), Affine.identity[D2])
      )
    val time = imageRight(Axis.create("time", 2, AxisKind.Time))
    val channel =
      imageRight(Axis.create("channel", 3, AxisKind.Channel))
    val axes = imageRight(NonSpatialAxes.from(Vector(time, channel)))
    val source =
      imageRight(
        Sampled.continuous(
          grid,
          axes,
          NDArray.tabulate[Double](2, 3, 2, 3)((i, j, t, c) =>
            1000.0 * i + 100.0 * j + 10.0 * t + c
          )
        )
      )
    val identity =
      source.mapValuesAs[Double, Continuous](value => value)
    val sequential =
      source
        .mapValuesAs[Double, Continuous](_ + 1.0)
        .mapValuesAs[Double, Continuous](_ * 2.0)
    val composed =
      source.mapValuesAs[Double, Continuous](value => (value + 1.0) * 2.0)
    val categorical =
      source.mapValuesAs[Int, Categorical](_.toInt)
    val replacement =
      imageRight(
        source.replaceDataChecked[Int, Categorical, ravel.Rank[4]](
          NDArray.zeros[Int](2, 3, 2, 3)
        )
      )
    val reduced =
      imageRight(
        source.replaceAfterNonSpatialReduction[
          Double,
          Continuous,
          ravel.Rank[3]
        ](
          axis = 0,
          reducedData = NDArray.tabulate[Double](2, 3, 3)((i, j, c) => 1000.0 * i + 100.0 * j + c)
        )
      )

    assert(identity.sameValuesAs(source)(_ == _))
    assert(sequential.sameValuesAs(composed)(_ == _))
    assert(categorical.sampleSpace eq source.sampleSpace)
    assert(replacement.sampleSpace eq source.sampleSpace)
    assertEquals(
      source.replaceDataChecked[Int, Categorical, ravel.Rank[3]](
        NDArray.zeros[Int](2, 3, 6)
      ),
      Left(
        ImageError.SampledShapeMismatch(
          Vector(2, 3, 2, 3),
          Vector(2, 3, 6)
        )
      )
    )
    assertEquals(reduced.logicalShape, Vector(2, 3, 3))
    assertEquals(
      reduced.nonSpatialAxes.records,
      Vector(channel.record)
    )
    assertEquals(
      source.replaceAfterNonSpatialReduction[
        Double,
        Continuous,
        ravel.Rank[3]
      ](1, NDArray.zeros[Double](2, 3, 4)),
      Left(
        ImageError.SampledShapeMismatch(
          Vector(2, 3, 2),
          Vector(2, 3, 4)
        )
      )
    )

  test("invalid maps and incompatible application fail explicitly"):
    assertEquals(
      LatticeMap.identity[D2](Vector(3)),
      Left(
        ImageError.LatticeMapRankMismatch(
          2, 1, 1, 2, 2, 2
        )
      )
    )
    assertEquals(
      LatticeMap.permute[D2](Vector(3, 4), Vector(0, 0)),
      Left(
        ImageError.InvalidLatticeAxisPermutation(Vector(0, 0), 2)
      )
    )
    assertEquals(
      LatticeMap.stridedPermutation[D2](
        Vector(3, 4),
        Vector(3, 4),
        Vector(0, 0),
        Vector(0, 1),
        Vector(1, 0)
      ),
      Left(ImageError.ZeroLatticeStep(1))
    )
    assertEquals(
      LatticeMap.stridedPermutation[D2](
        Vector(3, 4),
        Vector(2, 2),
        Vector(2, 0),
        Vector(0, 1),
        Vector(1, 1)
      ),
      Left(ImageError.LatticeMapOutOfBounds(0, 2L, 3L, 3))
    )
    assertEquals(
      LatticeMap.stride[D2](Vector(3, 4), Vector(1, -1)),
      Left(ImageError.InvalidSpatialStride(1, -1))
    )
    val map = imageRight(LatticeMap.identity[D2](Vector(3, 4)))
    assertEquals(
      map.sourceIndex(Vector(3, 0)),
      Left(ImageError.LatticeTargetIndexOutOfBounds(0, 3, 3))
    )
    val incompatible =
      imageRight(LatticeMap.identity[D2](Vector(2, 2)))
    assertEquals(
      map.followedBy(incompatible),
      Left(
        ImageError.LatticeMapCompositionMismatch(
          Vector(3, 4),
          Vector(2, 2)
        )
      )
    )
    val frame = geometryRight(Frame.named[D2]("bad-map-source"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(2, 2), Affine.identity[D2])
      )
    val image =
      imageRight(
        Sampled.continuous(
          grid,
          NonSpatialAxes.empty,
          NDArray.zeros[Double](2, 2)
        )
      )
    assertEquals(
      image.view(map),
      Left(
        ImageError.LatticeMapSourceShapeMismatch(
          Vector(2, 2),
          Vector(3, 4)
        )
      )
    )

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
