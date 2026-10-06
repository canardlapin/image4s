package image4s.geometry

class SharedSpatialAuthoritySuite extends munit.FunSuite:
  test("public coordinate names are exactly the spatial4s types"):
    val _ = summon[Dim =:= spatial4s.Dim]
    val _ = summon[D3 =:= spatial4s.D3]
    val _ = summon[Frame[D3] =:= spatial4s.Frame[spatial4s.D3]]
    val _ =
      summon[Point[Frame[D3], D3] =:= spatial4s.Point[spatial4s.Frame[spatial4s.D3], spatial4s.D3]]
    val _ =
      summon[Vec[Frame[D3], D3] =:= spatial4s.Vec[spatial4s.Frame[spatial4s.D3], spatial4s.D3]]
    val _ = summon[FrameKey =:= spatial4s.FrameKey]
    val _ = summon[FrameRecord =:= spatial4s.FrameRecord]

  test("a spatial4s frame and point cross an Image4s grid boundary without copying owners"):
    val frame = spatial4s.Frame.named[spatial4s.D3]("head").toOption.get
    val point = spatial4s.Point.in(frame)(1.0, 2.0, 3.0).toOption.get
    val grid = Grid.in(frame)(Vector(4, 5, 6), Affine.identity[D3]).toOption.get
    assert(grid.frame.sameRuntimeOwnerAs(frame))
    assert(point.belongsTo(grid.frame))
    val restored = Frame.align(frame, grid.frame).toOption.get.pointToRight(point).toOption.get
    assertEquals(restored.coordinates, point.coordinates)

  test("custom unit key encoding distinguishes scale and quantity semantics"):
    def unit(scale: Double, quantity: spatial4s.CoordinateQuantity) =
      spatial4s.CoordinateUnit.custom("custom", "c", quantity, Some(scale)).toOption.get
    val first = unit(0.01, spatial4s.CoordinateQuantity.Length)
    val second = unit(0.02, spatial4s.CoordinateQuantity.Length)
    val customQuantity = spatial4s.CoordinateQuantity.custom("length").toOption.get
    val third = unit(0.01, customQuantity)
    assertNotEquals(LengthUnit.serializedName(first), LengthUnit.serializedName(second))
    assertNotEquals(LengthUnit.serializedName(first), LengthUnit.serializedName(third))
    assertEquals(LengthUnit.serializedName(LengthUnit.Millimeter), "Millimeter")
