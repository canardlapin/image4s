package image4s.laws

import image4s.Axis
import image4s.AxisKind
import image4s.ImageError
import image4s.NonSpatialAxes
import image4s.Sampled
import image4s.apply
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import ravel.DType.given
import ravel.NDArray
import ravel.Shape
import image4s.geometry.Affine
import image4s.geometry.D3
import image4s.geometry.Frame
import image4s.geometry.GeometryError
import image4s.geometry.Grid
import image4s.geometry.Index

final class ImageRepresentationContractSuite extends ScalaCheckSuite:
  private val extents =
    for
      nx <- Gen.choose(2, 5)
      ny <- Gen.choose(2, 5)
      nz <- Gen.choose(2, 5)
      nt <- Gen.choose(2, 5)
    yield (nx, ny, nz, nt)

  property(
    "checked legacy ingress preserves every (i,j,k,t) logical value"
  ):
    forAll(extents): (nx, ny, nz, nt) =>
      val legacy = legacySeries(nx, ny, nz, nt)
      val canonical =
        NDArray.tabulate[Double](nx, ny, nz, nt)((i, j, k, t) =>
          legacy(legacyIndex4(nx, ny, nz, i, j, k, t))
        )
      val frame = geometryRight(Frame.named[D3]("legacy-parity"))
      val grid =
        geometryRight(
          Grid.in(frame)(
            Vector(nx, ny, nz),
            Affine.identity[D3]
          )
        )
      val time = imageRight(Axis.create("time", nt, AxisKind.Time))
      val axes = imageRight(NonSpatialAxes.from(Vector(time)))
      val sampled = imageRight(Sampled.scalar(grid, axes, canonical))

      assertEquals(sampled.logicalShape, Vector(nx, ny, nz, nt))
      assertEquals(
        signatureFromLegacy(legacy, nx, ny, nz, nt),
        signatureFromCanonical(canonical, nx, ny, nz, nt)
      )

      var i = 0
      while i < nx do
        var j = 0
        while j < ny do
          var k = 0
          while k < nz do
            var t = 0
            while t < nt do
              val expected = logicalValue(i, j, k, t)
              assertEquals(canonical(i, j, k, t), expected)
              assertEquals(sampled(i, j, k, t), expected)
              t += 1
            k += 1
          j += 1
        i += 1

      val naive =
        NDArray.fromSeq(Shape(nx, ny, nz, nt), legacy)
      assertNotEquals(
        naive(0, 0, 0, 1),
        logicalValue(0, 0, 0, 1),
        "direct adoption of a first-axis-fastest buffer must stay detectable"
      )

  test("immutable strided Ravel views remain valid zero-copy image storage"):
    val frame = geometryRight(Frame.named[D3]("strided-view"))
    val grid =
      geometryRight(
        Grid.in(frame)(Vector(4, 3, 2), Affine.identity[D3])
      )
    val base =
      NDArray.tabulate[Double](4, 3, 2)((i, j, k) =>
        logicalValue(i, j, k, 0)
      )
    val reversed = base.reverse(0)
    val sampled =
      imageRight(Sampled.scalar(grid, NonSpatialAxes.empty, reversed))

    assert(sampled.data eq reversed)
    assert(!sampled.data.isContiguous)
    assertEquals(
      sampled(0, 1, 1),
      base(3, 1, 1)
    )
    assert(reversed.reverse(0).sameElements(base))

  test("z is the third grid axis and not an anatomical world-axis claim"):
    val frame = geometryRight(Frame.named[D3]("permuted-world"))
    val affine =
      geometryRight(
        Affine.fromOriginSpacingDirection[D3](
          origin = Vector(0.0, 0.0, 0.0),
          spacing = Vector(1.0, 1.0, 1.0),
          directionRowMajor = Vector(
            0.0,
            0.0,
            1.0,
            0.0,
            1.0,
            0.0,
            1.0,
            0.0,
            0.0
          )
        )
      )
    val grid =
      geometryRight(Grid.in(frame)(Vector(2, 3, 4), affine))
    val sampled =
      imageRight(
        Sampled.scalar(
          grid,
          NonSpatialAxes.empty,
          NDArray.tabulate[Double](2, 3, 4)((_, _, k) => k.toDouble)
        )
      )
    val index = geometryRight(Index.of[D3](1, 2, 3))
    val world = geometryRight(grid.pointAt(index))

    assertEquals(sampled(1, 2, 3), 3.0)
    assertEquals(world.coordinates, Vector(3.0, 2.0, 1.0))
    assertNotEquals(world.coordinates(2), 3.0)

  private final case class Signature(
      samples: Int,
      sum: Double,
      weightedSum: Double
  )

  private def signatureFromLegacy(
      legacy: Array[Double],
      nx: Int,
      ny: Int,
      nz: Int,
      nt: Int
  ): Signature =
    var sum = 0.0
    var weighted = 0.0
    var samples = 0
    var i = 0
    while i < nx do
      var j = 0
      while j < ny do
        var k = 0
        while k < nz do
          var t = 0
          while t < nt do
            val value =
              legacy(legacyIndex4(nx, ny, nz, i, j, k, t))
            sum += value
            weighted +=
              value * (cIndex4(ny, nz, nt, i, j, k, t) + 1).toDouble
            samples += 1
            t += 1
          k += 1
        j += 1
      i += 1
    Signature(samples, sum, weighted)

  private def signatureFromCanonical(
      canonical: ravel.Array4[Double],
      nx: Int,
      ny: Int,
      nz: Int,
      nt: Int
  ): Signature =
    var sum = 0.0
    var weighted = 0.0
    var samples = 0
    var i = 0
    while i < nx do
      var j = 0
      while j < ny do
        var k = 0
        while k < nz do
          var t = 0
          while t < nt do
            val value = canonical(i, j, k, t)
            sum += value
            weighted +=
              value * (cIndex4(ny, nz, nt, i, j, k, t) + 1).toDouble
            samples += 1
            t += 1
          k += 1
        j += 1
      i += 1
    Signature(samples, sum, weighted)

  private def legacySeries(
      nx: Int,
      ny: Int,
      nz: Int,
      nt: Int
  ): Array[Double] =
    val values = new Array[Double](nx * ny * nz * nt)
    var t = 0
    while t < nt do
      var k = 0
      while k < nz do
        var j = 0
        while j < ny do
          var i = 0
          while i < nx do
            values(legacyIndex4(nx, ny, nz, i, j, k, t)) =
              logicalValue(i, j, k, t)
            i += 1
          j += 1
        k += 1
      t += 1
    values

  private def legacyIndex4(
      nx: Int,
      ny: Int,
      nz: Int,
      i: Int,
      j: Int,
      k: Int,
      t: Int
  ): Int =
    i + nx * (j + ny * (k + nz * t))

  private def cIndex4(
      ny: Int,
      nz: Int,
      nt: Int,
      i: Int,
      j: Int,
      k: Int,
      t: Int
  ): Int =
    t + nt * (k + nz * (j + ny * i))

  private def logicalValue(i: Int, j: Int, k: Int, t: Int): Double =
    i.toDouble +
      10.0 * j.toDouble +
      100.0 * k.toDouble +
      1000.0 * t.toDouble

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
