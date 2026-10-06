package image4s.geometry

final class AffineFiniteSuite extends munit.FunSuite:
  test("checked affine application refuses overflow of finite inputs"):
    val affine =
      Affine.fromRowMajor[D2](Vector(2.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0)).toOption.get
    val overflow = affine(Vector(Double.MaxValue, 0.0))
    assert(overflow.left.exists {
      case GeometryError.NonFiniteCoordinate(0, value) => value.isPosInfinity
      case _ => false
    })
    assertEquals(affine(Vector(1.0, 2.0)), Right(Vector(2.0, 2.0)))
